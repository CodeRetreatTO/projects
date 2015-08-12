var startSequence = 'abc';

var rules = [
    {
        replaceTarget: 'a',
        replaceWith: 'b'
    },
    {
        replaceTarget: 'b',
        replaceWith: 'cacab'
    }
];

function replaceChars(element, index, array){
    var newSequence = startSequence;
    newSequence = newSequence.replace(element.replaceTarget, element.replaceWith);
    return newSequence;
}

var process = function() {
    rules.forEach(replaceChars);
}();
