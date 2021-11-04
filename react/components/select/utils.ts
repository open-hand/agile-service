import { SelectConfig } from '@/hooks/useSelect';

export function wrapRequestCallback(request: SelectConfig<any>['request'], callback?: (val: Parameters<SelectConfig['request']>[0]) => void): SelectConfig<any>['request'] {
  return async (requestData) => {
    const res = await request(requestData);
    callback && await callback(requestData);
    return res;
  };
}
export function refsBindRef(...refs: any[]) {
  return function bind(originRef: any) {
    refs.forEach((r) => {
      if (typeof r === 'function') {
        r(originRef);
      } else if (r && typeof r === 'object' && Object.keys(r).includes('current')) {
        Object.assign(r, {
          current: originRef,
        });
      }
    });
  };
}
