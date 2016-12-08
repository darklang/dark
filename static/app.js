/*
 * How is the next application state calculated,
 * given the current state and the action?
 */
const counter = (state = 0, action) => {
  switch (action.type) {
  case 'INCREMENT':
    return state + 1;
  case 'DECREMENT':
    return state - 1;
  default:
    return state;
  }
}

/*
 * What does UI look like, assuming it doesn't know
 * about the state or actions, and is a function
 * of the props?
 */
const Counter = ({
  value,
  onIncrement,
  onDecrement
}) => (
    <div>
    <h1>{value}</h1>
    <button onClick={onIncrement}>+</button>
    <button onClick={onDecrement}>-</button>
    </div>
);

/*
 * Which injected props should be calculated
 * from the application state and how?
 */
const mapStateToProps = (state) => {
  return {
    value: state
  };
}

/*
 * Which injected props should be callbacks
 * that dispatch actions, and which actions?
 */
const mapDispatchToProps = (dispatch) => {
  return {
    onIncrement: () => {
      dispatch({
        type: 'INCREMENT'
      })
    },
    onDecrement: () => {
      dispatch({
        type: 'DECREMENT'
      })
    }
  };
};

/*
 * Let's create a container component
 * that injects props into the pure UI component
 * according to the instructions above, but
 * instead of all those props, accepts a store.
 */
const { connect } = ReactRedux;
const CounterContainer = connect(
  mapStateToProps,
  mapDispatchToProps
)(Counter);

/*
 * Let's create a store.
 */
const { createStore } = Redux;
const store = createStore(counter);

/*
 * Finally, render the container,
 * passing the store to it.
 */
ReactDOM.render(
    <CounterContainer store={store} />,
  document.getElementById('root')
);
