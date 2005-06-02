  ? def makeSortedQueue := <import:org.cubik.cle.prim.makeSortedQueue>
  # value: <makeSortedQueue>
  
  ? def q := makeSortedQueue()
  # value: <sorted queue of 0>
  
  ? q.put(10, 'a')
  
  ? q.peek(thunk { 0 })
  # value: [10, 'a']

  ? q.pop()
  # value: [10, 'a']
  
  ? q.peek(thunk { 0 })
  # value: 0

  ? q.put(10, 'a'); q.asList()
  # value: [[10, 'a']]

  ? q.put(20, 'b'); q.asList()
  # value: [[10, 'a'], [20, 'b']]

  ? q.put(14, 'c'); q.asList()
  # value: [[10, 'a'], [14, 'c'], [20, 'b']]

  ? q.put(15, 'd'); q.asList()
  # value: [[10, 'a'], [14, 'c'], [15, 'd'], [20, 'b']]

  ? q.put(9,  'e'); q.asList()
  # value: [[9, 'e'], [10, 'a'], [14, 'c'], [15, 'd'], [20, 'b']]

  ? q.put(21, 'f'); q.asList()
  # value: [[9, 'e'], [10, 'a'], [14, 'c'], [15, 'd'], [20, 'b'], [21, 'f']]

  ? q.pop()
  # value: [9, 'e']

  ? q.asList()
  # value: [[10, 'a'], [14, 'c'], [15, 'd'], [20, 'b'], [21, 'f']]