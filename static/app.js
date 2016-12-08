const graph = (state=[], action) => {
  switch (action.type) {
  case 'CREATE':
    return state.concat([{
      name: "tbc" + state.length,
      x: action.location.x,
      y: action.location.y
    }])
  default:
    return state;
  }
}

class Node extends React.Component {
  render() {
    return (
        <g transform={"translate(" + this.props.x + ", " + this.props.y + ")"}>
        <rect width={100} height={50} fill="red" />
        <text fill="blue">Hello!</text>
        <text fill="black">More</text>
        </g>
    )
  }
}

const Graph = ({value, onCreate}) => (
    <svg onClick={onCreate} style={{width: "100%", height: "100%"}}>
    {value.map(node => <Node key={node.name} x={node.x} y={node.y}>{node.name}</Node>)}
    </svg>
);

const mapStateToProps = (state) => {
  return {value: state};
}

const mapDispatchToProps = (dispatch) => {
  return {
    onCreate: (event) => {
      dispatch({
        type: 'CREATE',
        location: {x: event.clientX, y: event.clientY}
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
