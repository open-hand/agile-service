import { useRef } from 'react';
import { refsBindRef } from '@/components/select/utils';

export function useRefsBindRef<RT = any>(...refs: any[]) {
  const ref = useRef<(r: RT) => void>();
  ref.current = refsBindRef(...refs);
  return ref;
}
