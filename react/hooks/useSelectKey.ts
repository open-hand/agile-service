import { useState, useMemo, useRef } from 'react';

interface Config {
  value: string[]
}

export default function useSelectKey({
  value,

}: Config) {
  const ref = useRef();
  const [key, updateKey] = useState(0);
  useMemo(() => {
    if (ref.current) {
      // 有新的未加载的值，就重新设置key
      const hasNewUnExistValue = value.some((v) => !ref.current.options.find((record) => record.get('issueId') === v));
      if (hasNewUnExistValue) {
        updateKey((k) => k + 1);
      }
    }
  }, [value]);

  return {
    key,
    ref,
  };
}
