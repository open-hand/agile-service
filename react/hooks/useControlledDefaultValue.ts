import {
  useEffect, useState, Dispatch, SetStateAction,
} from 'react';

/**
 * 当defaultValue变化时，自动set
 * @param defaultValue
 */
export default function useControlledDefaultValue<T>(defaultValue: T):[T, Dispatch<SetStateAction<T>>] {
  const [value, setValue] = useState<T>(defaultValue);
  useEffect(() => {
    setValue(defaultValue);
  }, [defaultValue]);
  return [value, setValue];
}
