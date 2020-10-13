import React from 'react';
import { isEqual } from 'lodash';

function useDeepCompareMemoize(value: any) {
  const ref = React.useRef();

  if (!isEqual(value, ref.current)) {
    ref.current = value;
  }

  return ref.current;
}

function useDeepCompareEffect(callback: React.EffectCallback, dependencies: React.DependencyList) {
  // eslint-disable-next-line react-hooks/exhaustive-deps
  React.useEffect(callback, useDeepCompareMemoize(dependencies));
}

export function useDeepCompareEffectNoCheck(callback: React.EffectCallback, dependencies: React.DependencyList) {
  // eslint-disable-next-line react-hooks/exhaustive-deps
  React.useEffect(callback, useDeepCompareMemoize(dependencies));
}

export default useDeepCompareEffect;
