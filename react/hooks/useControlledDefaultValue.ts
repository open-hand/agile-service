import {
  useState, Dispatch, SetStateAction,
} from 'react';
import useDeepCompareEffect from './useDeepCompareEffect';
/**
 * 当defaultValue变化时，自动set
 * @param defaultValue
 */
export default function useControlledDefaultValue<T>(defaultValue: T):[T, Dispatch<SetStateAction<T>>] {
  const [value, setValue] = useState<T>(defaultValue);
  // 深比较依赖，因为有时候依赖是对象
  useDeepCompareEffect(() => {
    setValue(defaultValue);
  }, [defaultValue]);
  return [value, setValue];
}
