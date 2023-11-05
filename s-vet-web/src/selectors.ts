import {
  MutableRefObject,
  SetStateAction,
  useCallback,
  useEffect,
  useRef,
  useState,
} from "react";
import { useMediaQuery } from "react-responsive";

export const useIs2Columns = () =>
  useMediaQuery({ query: "(min-width: 1600px)" });

export const useStateWithPromise = <T>(
  initialState: T
): [T, (_: SetStateAction<T>) => Promise<unknown>] => {
  const [state, setState] = useState({ value: initialState });
  const resolverRef: MutableRefObject<((_: T) => void) | null> = useRef<
    ((_: T) => void) | null
  >(null);

  useEffect(() => {
    if (resolverRef.current) {
      resolverRef.current(state.value);
      resolverRef.current = null;
    }
    /**
     * Since a state update could be triggered with the exact same state again,
     * it's not enough to specify state as the only dependency of this useEffect.
     * That's why resolverRef.current is also a dependency, because it will guarantee,
     * that handleSetState was called in previous render
     */
  }, [resolverRef.current, state]);

  const handleSetState = useCallback(
    (stateAction: SetStateAction<T>) => {
      if (stateAction instanceof Function) {
        setState((s) => ({ value: stateAction(s.value) }));
      } else {
        setState({ value: stateAction });
      }
      return new Promise((resolve) => {
        resolverRef.current = resolve;
      });
    },
    [setState]
  );

  return [state.value, handleSetState];
};
