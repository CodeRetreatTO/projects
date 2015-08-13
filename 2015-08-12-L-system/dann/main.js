var start = 'a'

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

//// RENDERER


function deg_to_rad(deg) {
  return 2 * Math.PI * (deg / 360)
}

function render(str, rad, size) {
  // mock turtle graphics: + and - mean turn, anything else moves forward

  var canvas = document.getElementById('can')
  var ctx = canvas.getContext('2d')

  ctx.clearRect(0, 0, 2000, 2000)

  var x = 0
  var y = 0
  var tau = 2*Math.PI
  var angle = 0

  ctx.beginPath()
  ctx.moveTo(x, y)

  str.split('').forEach(function(char) {
    if(char == '+') return angle += rad
    if(char == '-') return angle -= rad

    if(angle > tau) angle -= tau
    if(angle < 0)   angle += tau

    x += Math.sin(angle) * size
    y += Math.cos(angle) * size

    ctx.lineTo(x, y)
  })

  ctx.stroke()
}
