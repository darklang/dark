const graph = (state=[], action) => {
  switch (action.type) {
  case 'CREATE':
    return state.concat([{name: "tbc" + state.length}])
  default:
    return state;
  }
}

const Graph = ({value, onCreate}) => (
    <div onClick={onCreate} style={{height: "100%"}}>
    {value.map(node => <h1 key={node.name}>{node.name}</h1>)}
    <button onClick={onCreate}>+</button>
    </div>
);

const mapStateToProps = (state) => {
  return {value: state};
}

const mapDispatchToProps = (dispatch) => {
  return {
    onCreate: () => {
      dispatch({
        type: 'CREATE'
      })
    }
  };
};

const GraphContainer = ReactRedux.connect(mapStateToProps, mapDispatchToProps)(Graph);
const store = Redux.createStore(graph, []);

ReactDOM.render(
    <GraphContainer store={store} />,
  document.getElementById('root')
);
