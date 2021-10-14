import { usePersistFn } from 'ahooks';
import { useRef, useState, useMemo } from 'react';
import { noop, set } from 'lodash';
import openCreateIssue from '@/components/create-issue';

interface IQuickCreateIssueHookData {
  isCreate: boolean

}
interface IQuickCreateIssueHookComponentProps {
  onCreateChange: any
  cantCreateEvent: () => void
  onCreate: any
  typeIdChange: (id: any) => void
  summaryChange: any
  assigneeChange: any
  setDefaultSprint: any
}
interface IQuickCreateIssueHookConfig {
  onCreate?: (res: any, isQuickCreate: boolean) => void
}
function useQuickCreateIssue({ onCreate = noop }: IQuickCreateIssueHookConfig): [IQuickCreateIssueHookData, IQuickCreateIssueHookComponentProps] {
  const [isCreate, setIsCreate] = useState(false);
  const quickCreateDataRef = useRef<any>({});
  const handleFinishCreate = usePersistFn((res: any, isQuickCreate: boolean) => {
    onCreate(res, isQuickCreate);
  });
  const handleCantCreateEvent = usePersistFn(() => {
    openCreateIssue({
      ...quickCreateDataRef.current,
      onCreate: (issue) => onCreate(issue, false),
    });
  });
  const handleChangeQuickCreateData = usePersistFn((key: string, value: any) => {
    let defaultKey = key;
    if (['summary', 'sprint'].includes(key)) {
      defaultKey = `defaultValues.${key}`;
    }
    set(quickCreateDataRef.current, defaultKey, value);
  });

  const props = useMemo(() => ({
    onCreateChange: setIsCreate,
    cantCreateEvent: handleCantCreateEvent,
    onCreate: (res: any) => handleFinishCreate(res, true),
    typeIdChange: (id: any) => {
      handleChangeQuickCreateData('defaultTypeId', id);
    },
    summaryChange: (issueSummary: any) => {
      handleChangeQuickCreateData('summary', issueSummary);
    },
    assigneeChange: (assigneeId: string, assignee: any) => {
      handleChangeQuickCreateData('defaultAssignee', assignee);
    },
    setDefaultSprint: (value: any) => {
      handleChangeQuickCreateData('sprint', value);
    },

  }), [handleCantCreateEvent, handleChangeQuickCreateData, handleFinishCreate]);
  return [{ isCreate }, props];
}

export default useQuickCreateIssue;
