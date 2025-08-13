const FUNCTION_PLOT_ID_NAME = "graph";
let Inputs = {};
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

const getWeights = (names) => names.map(n => {
    const safeName = n.toLowerCase().replaceAll(' ','_');
    return Inputs[safeName]['weight'].value()[1];
});

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

function createDoubleSlider(name, title, type, root, options) {
    const rangeSliderDiv = document.createElement("div");
    rangeSliderDiv.style['width'] = "200px";
    rangeSliderDiv.style['margin'] = "30px"
    rangeSliderDiv.id = `${name}_${type}_range_slider`;

    const left = document.createElement("output");
    const right = document.createElement("output");

    left.id = `${name}_${type}_left`;
    right.id = `${name}_${type}_right`;
    left.value = options.value[0];
    right.value = options.value[1];


    // Proper container maybe?
    root.append(left)
    root.append(rangeSliderDiv);
    root.append(right)
    const eventOptions = {
        ...options,
        onInput: (e) => {
            drawMathPlot(name);
            left.value = e[0];
            right.value = e[1];
        },
    }

    // We can't run this until the root div mounts to the page.
    // So here's a callback.

    return () => {
        Inputs[name][type] = rangeSlider(rangeSliderDiv, eventOptions);
    }
}

function createSingleSlider(name, title, type, root, options) {
    const rangeSliderDiv = document.createElement("div");
    rangeSliderDiv.style['width'] = "200px";
    rangeSliderDiv.style['margin'] = "30px"
    rangeSliderDiv.className = "single-selector"
    rangeSliderDiv.id = `${name}_${type}_range_slider`;

    const value = document.createElement("output");

    value.id = `${name}_${type}_value`;
    value.value = options.value[1];


    // Proper container maybe?
    root.append(rangeSliderDiv);
    root.append(value)
    const eventOptions = {
        ...options,
        thumbsDisabled: [true, false],
        rangeSlideDisabled: true,
        // Is this called on reading value?
        onInput: (e) => {
            drawMathPlot(name);
            value.value = e[1];
        },
    }

    return () => {
        Inputs[name][type] = rangeSlider(rangeSliderDiv, eventOptions);
    }
}

document.getElementById("set_category").onclick =(_) => {
    const schema = document.getElementById("schema").value
    fetch(window.location.origin + "/schema/" + schema)
        .then(r => r.json())
        .then(j => {
            document.getElementById("selectors").innerHTML = "";
            document.getElementById("results").innerHTML = "";
            document.getElementById("results").innerText = "";
            const zipped = zip([j["names"], j["parameters"], j["types"]]);
            const createSliderCallbacks = [];
            zipped.forEach(element => {
                const name = element[0];
                const meta = element[1];
                const type = element[2];


                const safeName = name.toLowerCase().replaceAll(' ','_');
                Inputs[safeName] = {
                    meta: meta,
                    type: type
                }


                switch (type) {
                    case "math":
                        const selectorM = document.createElement("div");
                        selectorM.append(name);
                        createSliderCallbacks.push(
                            createDoubleSlider(
                                safeName,
                                "Selection",
                                "selection",
                                selectorM,
                                {
                                    min: meta[0],
                                    max: meta[1],
                                    step: 1,
                                    value: [meta[0], meta[1]],
                                })
                        )
                        createSliderCallbacks.push(
                            createSingleSlider(
                                safeName,
                                "Preferred direction",
                                "direction",
                                selectorM,
                                {
                                    min: -1,
                                    max: 1,
                                    step: 1,
                                    value: [-1, 0]
                                }
                            )
                        )
                        createSliderCallbacks.push(
                            createSingleSlider(
                                safeName,
                                "Harshness",
                                "harshness",
                                selectorM,
                                {
                                    min: 0.1,
                                    max: 10,
                                    step: 0.1,
                                    value: [0.1, 5]
                                }
                            )
                        )
                        // Make this vertical, to the side?
                        createSliderCallbacks.push(
                            createSingleSlider(
                                safeName,
                                "Weight",
                                "weight",
                                selectorM,
                                {
                                    min: 0.1,
                                    max: 10,
                                    step: 0.1,
                                    value: [0.1, 1],
                                }
                            )
                        )
                        const graphDiv = document.createElement("div");
                        graphDiv.id = safeName + "_" + FUNCTION_PLOT_ID_NAME;
                        selectorM.append(graphDiv);
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
                            input.id = safeName + "_" + item;
                            label.for = safeName + "_" + item;
                            label.innerText = item;
                            fieldset.append(input);
                            fieldset.append(label);
                        }
                        selector.append(fieldset);

                        createSliderCallbacks.push(
                            createSingleSlider(
                                safeName,
                                "Weight",
                                "weight",
                                selector,
                                {
                                    min: 0.1,
                                    max: 10,
                                    step: 0.1,
                                    value: [0.1, 1],
                                }
                            )
                        )
                        selector.append(document.createElement("hr"))
                        document.getElementById("selectors").append(selector);
                        break;
                }
            });
            // Now that all slider divs have been mounted to the root, initialize them as range sliders.
            createSliderCallbacks.forEach(callback => callback());
            // Do the same for math plots once these mount
            Object.keys(Inputs).filter(key => Inputs[key].type === 'math').forEach(drawMathPlot)
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

                    const safeName = name.toLowerCase().replaceAll(' ','_');


                    data.weights.push(Number(document.getElementById(safeName + "_weight").value));
                    switch (type) {
                        case "math":
                            data.selections.push([
                                Number(document.getElementById(safeName + "_low").value),
                                Number(document.getElementById(safeName + "_high").value),
                                Number(document.getElementById(safeName + "_harshness").value),
                                Number(document.getElementById(safeName + "_direction").value)
                            ]);
                            break;
                        default:
                            const boxes = meta.map(item => document.getElementById(safeName + "_" + item));
                            const checkedBoxes = boxes.filter(b => b.checked);
                            const names = checkedBoxes.map(b => b.value);
                            data.selections.push(names);
                    }
                });
                fetch(window.location.origin + "/" + document.getElementById("schema").value, {
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

function drawMathPlot(name) {
    const graphDivName = '#' + name + '_' + FUNCTION_PLOT_ID_NAME;
    const graphWeight = Inputs[name]['weight'].value()[1];
    const lowerBound = Inputs[name]['selection'].min();
    const upperBound = Inputs[name]['selection'].max();
    const [selectedLow, selectedHigh] = Inputs[name]['selection'].value();
    const harshness = Inputs[name]['harshness'].value()[1];
    const direction = Inputs[name]['direction'].value()[1];

    const spread = upperBound - lowerBound;
    const safeHarshness = (harshness === 0) ? 0.1 : harshness;

    const stdDev = spread / safeHarshness;
    const safeStdDev = (-1 < stdDev && stdDev < 1) ? 1 : stdDev;

    const lowerFunction = `exp(-1 * (${selectedLow} - x)^2 / (2 * ${safeStdDev}^2))`;
    const upperFunction = `exp(-1 * (x - ${selectedHigh})^2 / (2 * ${safeStdDev}^2))`;

    let directedLowerFunction = lowerFunction;
    let directedUpperFunction = upperFunction;
    let bottomColor = 'orange';
    let topColor = 'orange';
    let middleColor = 'green';

    // Non-integer exponents are against the law.  Straight to jail.
    const intHarshness = Math.trunc(harshness)

    switch (direction) {
        case 1:
            // Make the results more gentle
            directedUpperFunction = `nthRoot(${upperFunction}, ${harshness})`;
            topColor = 'blue';
            // Make the results more harsh
            directedLowerFunction = `(${lowerFunction})^${intHarshness}`;
            bottomColor = 'red';
            break;
        case -1:
            // Make the results more gentle
            directedLowerFunction = `nthRoot(${lowerFunction},${harshness})`;
            bottomColor = 'blue';
            // Make the results more harsh
            directedUpperFunction = `(${upperFunction})^${intHarshness}`;
            topColor = 'red';
            break;
    }

    functionPlot({
        target: graphDivName,
        data: [
            { fn: `${graphWeight} * ${directedLowerFunction}`, range: [lowerBound, selectedLow], color: bottomColor},
            { fn: `${graphWeight}`, range: [selectedLow, selectedHigh], color: middleColor},
            { fn: `${graphWeight} * ${directedUpperFunction}`, range: [selectedHigh, upperBound], color: topColor}
        ],
        xAxis: { domain: [Number(lowerBound), Number(upperBound)] },
        yAxis: { domain: [0, 12]},
        disableZoom: true
    })
}

