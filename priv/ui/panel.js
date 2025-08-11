const zip = (lists) => {
    const tupleSize = lists.length;
    const listLength = lists[0].length;
    const result = new Array(listLength);
    for (let listPosition = 0 ; listPosition < listLength ; listPosition ++ ) {
        const tuple = new Array(tupleSize);
        for (let tuplePosition = 0 ; tuplePosition < lists.length ; tuplePosition ++ ) {
            tuple[tuplePosition] = lists[tuplePosition][listPosition];
        }
        result[listPosition] = tuple;
    }
    return result;
}

const getWeights = (names) => names.map(n => Number(document.getElementById(n + "_weight").value));

const addResults = (results, names) => {
    const r = document.getElementById("results");
    r.innerHTML = "";
    r.append(document.createElement("hr"))
    const weights = getWeights(names);
    for (const result of results) {
        const name = result[1];
        const score = result[2];
        const options = result[3];
        const row = document.createElement("div");
        row.append("Name: " + name);
        row.append(document.createElement("br"))
        row.append(asPercent(score, 1) + " match");
        row.append(document.createElement("br"))
        row.append("----")
        row.append(document.createElement("br"))
        for (let i = 0 ; i < options.length ; i ++) {
            const option = options[i];
            const n = names[i];
            row.append(n + ": ")
            row.append(option[0].toString())
            row.append(", " + asPercent(option[1], weights[i]) + " match")
            row.append(document.createElement("br"))
        }
        row.append(document.createElement("hr"))
        r.append(row);
    }
}

const asPercent = (numString, weight) => {
    const float = Number(numString);
    const multiplied = (float * 100) / weight;
    const rounded = multiplied.toFixed(2);
    return rounded.toString().replace(/\.0+$/, '') + "%";
}

const createNumberSlider = (min, max, idFor, val, title, addTo, step) => {
    const range = document.createElement("input");
    range.type = "range";
    range.min = min;
    range.max = max;
    range.id = idFor;
    range.value = val;
    range.step = step;
    range.oninput= (e) => document.getElementById(idFor + "_output").value = e.target.value;
    const p = document.createElement("output");
    p.id = idFor + "_output";
    p.value = val;
    addTo.append(title);
    addTo.append(range);
    addTo.append(p);
}

document.getElementById("set_category").onclick =(_) => {
    const schema = document.getElementById("schema").value
    fetch("https://softsort.org/schema/" + schema)
        .then(r => r.json())
        .then(j => {
            document.getElementById("selectors").innerHTML = "";
            document.getElementById("results").innerHTML = "";
            document.getElementById("results").innerText = "";
            const zipped = zip([j["names"], j["parameters"], j["types"]]);
            zipped.forEach(element => {
                const name = element[0];
                const meta = element[1];
                const type = element[2];
                switch (type) {
                    case "math":
                        const selectorM = document.createElement("div");
                        selectorM.append(name);
                        selectorM.append(document.createElement("br"))
                        createNumberSlider(meta[0],meta[1], name + "_low", meta[0], "Lower bound", selectorM,1);
                        selectorM.append(document.createElement("br"))
                        createNumberSlider(meta[0], meta[1], name + "_high", meta[1], "Upper bound", selectorM,1);
                        selectorM.append(document.createElement("br"));
                        createNumberSlider(-1, 1, name + "_direction", 0, "Preferred direction", selectorM, 1);
                        selectorM.append(document.createElement("br"));
                        createNumberSlider(0.1, 10, name + "_harshness", 5, "Harshness", selectorM, 0.1);
                        selectorM.append(document.createElement("br"));
                        createNumberSlider(0.1, 10, name + "_weight", 1, "Weight", selectorM, 0.1);
                        selectorM.append(document.createElement("hr"))
                        document.getElementById("selectors").append(selectorM);
                        break;
                    default:
                        const selector = document.createElement("div");
                        selector.append(name)
                        const fieldset = document.createElement("fieldset");
                        for (const item of meta) {
                            const input = document.createElement("input");
                            const label = document.createElement("label");
                            input.type = "checkbox";
                            input.name = name;
                            input.value = item;
                            input.id = name + "_" + item;
                            label.for = name + "_" + item;
                            label.innerText = item;
                            fieldset.append(input);
                            fieldset.append(label);
                        }
                        selector.append(fieldset);
                        createNumberSlider(0.1, 10, name + "_weight", 1, "Weight", selector,0.1);
                        selector.append(document.createElement("hr"))
                        document.getElementById("selectors").append(selector);
                        break;
                }
            });
            const n = document.createElement("input");
            n.type = "number"
            n.min = 0
            n.max = 1000
            n.id = "count"
            n.value = 10
            document.getElementById("selectors").append(n);
            const button = document.createElement("button");
            button.innerText = "Submit";
            button.onclick = (_) => {
                const data = {
                    "selections": [],
                    "weights": [],
                    "k": parseInt(document.getElementById("count").value)
                };
                zipped.forEach(item => {
                    const name = item[0];
                    const meta = item[1];
                    const type = item[2];

                    data.weights.push(Number(document.getElementById(name + "_weight").value));
                    switch (type) {
                        case "math":
                            data.selections.push([
                                Number(document.getElementById(name + "_low").value),
                                Number(document.getElementById(name + "_high").value),
                                Number(document.getElementById(name + "_harshness").value),
                                Number(document.getElementById(name + "_direction").value)
                            ]);
                            break;
                        default:
                            const boxes = meta.map(item => document.getElementById(name + "_" + item));
                            const checkedBoxes = boxes.filter(b => b.checked);
                            const names = checkedBoxes.map(b => b.value);
                            data.selections.push(names);
                    }
                });
                console.log(data);
                fetch("https://softsort.org/" + document.getElementById("schema").value, {
                    method: 'POST',
                    headers: {
                        'Content-Type': 'application/json',
                    },
                    body: JSON.stringify(data),
                }).then((response) => response.json())
                    .then((d) => {
                        addResults(d["winners"], d["names"])
                    });

            }
            document.getElementById("selectors").append(button);
        })
}
