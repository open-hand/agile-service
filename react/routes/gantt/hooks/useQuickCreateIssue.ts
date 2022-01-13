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
  isCanQuickCreate?: (createData: any) => boolean
}
function defaultIsCanQuickCreate() {
  return true;
}
function useQuickCreateIssue({ onCreate = noop, isCanQuickCreate: propsIsCanQuickCreate = defaultIsCanQuickCreate }: IQuickCreateIssueHookConfig): [IQuickCreateIssueHookData, IQuickCreateIssueHookComponentProps] {
  const [isCreate, setIsCreate] = useState(false);
  const quickCreateDataRef = useRef<any>({});
  const handleFinishCreate = usePersistFn((res: any, isQuickCreate: boolean) => {
    onCreate(res, isQuickCreate);
  });
  const isCanQuickCreate = usePersistFn((createData: any) => propsIsCanQuickCreate(createData));
  const handleCantCreateEvent = usePersistFn(() => {
    openCreateIssue({
      isProgram: false,
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
    isCanQuickCreate,
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

  }), [handleCantCreateEvent, handleChangeQuickCreateData, handleFinishCreate, isCanQuickCreate]);
  return [{ isCreate }, props];
}

export default useQuickCreateIssue;
