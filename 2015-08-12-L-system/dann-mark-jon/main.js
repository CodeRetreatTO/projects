canvas = document.getElementById('can')
ctx = canvas.getContext('2d')

var size = 2

function render(str) {
    var angle = 0
    var x = 0
    var y = 0

    var long = 0.86602540378
    var short = 0.5

    ctx.beginPath()
    ctx.moveTo(x, y)

    str.split('').forEach(function(char) {
        if(char == '+') angle += 60
        if(char == '-') angle -= 60

        angle = angle % 360

        if(angle < 0) angle += 360

        if(char == 'a' || char == 'b') {
            if(angle == 0) {
                x += size
            }
            else if(angle == 60) {
                x += short*size
                y += long*size
            }
            else if(angle == 120) {
                x -= short*size
                y += long*size
            }
            else if(angle == 180) {
                x -= size
            }
            else if(angle == 240) {
                x -= short*size
                y -= long*size
            }
            else if(angle == 300) {
                x += short*size
                y -= long*size
            }

            ctx.lineTo(x, y)
        }
    })

    ctx.stroke()
}


var x = 'a'

var pretty_rules = { 'a' : '+b-a-b+'
                   , 'b ' : '-a+b+a-'
                   // , 'b' : 'a'
                   // , 'b<a>b' : 'aaa'
                   // , 'a<a>a' : 'c'
                   }

var rules = transform_rules(pretty_rules)

function transform_rules(rules) {
    var keys = Object.keys(rules)
    return keys.map(function(key) {
        old_key = key
        key = key.trim()
        return { replacement: rules[old_key]
               , key:    key
               , char:   key.split('>')[0].split('<').reverse()[0]
               , after:  key.split('>')[1] || ""           // get after  context
               , before: key.split('<').reverse()[1] || "" // get before context
               }
    })
}



function step(s) {
   return s.split('')
           .map(apply_rules)
           .join('')
}

function apply_rules(char, index, array) {
    var matches = find_matching_rules(char, index, array)
    var output = pick_and_apply_rule(matches) || char
    // console.log(char, output)
    return output
}

function find_matching_rules(c, i, a) {
    return rules.filter(function(rule) {
        return rule.char   == c
            && rule.after  == a.slice(i+1, i+1+rule.after.length).join('')
            && rule.before == a.slice(i-rule.before.length, i   ).join('')
    })
}

function pick_and_apply_rule(matches) {
    return (matches.sort(function(a, b) {
        return (b.key.length - a.key.length) || (Math.random() - 0.5) })[0]||{}).replacement
}

function lsystem(s, n) {
  return n ? lsystem(step(s), n-1)
           : s
}
