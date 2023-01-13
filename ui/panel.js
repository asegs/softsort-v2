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

document.getElementById("set_category").onclick =(_) => {
    const schema = document.getElementById("schema").value
    fetch("http://localhost:8081/schema/" + schema)
        .then(r => r.json())
        .then(j => {
            document.getElementById("selectors").innerHTML = "";
            const zipped = zip([j["names"], j["parameters"], j["types"]]);
            zipped.forEach(element => {
                const name = element[0];
                const meta = element[1];
                const type = element[2];
                switch (type) {
                    case "list":
                        //Create div...
                        break;

                }
            })
        })
}