import { useRef, useState } from 'react';

function useQuickCreateIssue() {
  const [isCreate, setIsCreate] = useState(false);
  const quickCreateDataRef = useRef<any>({});
}
