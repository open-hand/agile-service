import React, { useCallback, useEffect, useMemo } from 'react';
import { observer } from 'mobx-react-lite';
import { Modal, DataSet, Form } from 'choerodon-ui/pro';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { IModalProps, Issue } from '@/common/types';
import { issueApi } from '@/api';
import SelectParentIssue from '@/components/SelectParentIssue';

interface Props {
  issueId: string
  objectVersionNumber: number
  issueNum: string
  modal?: IModalProps
  projectId?:string
  onOk: (issue: Issue) => void
}

const ChangeParent: React.FC<Props> = ({
  issueId, objectVersionNumber, modal, onOk, projectId,
}) => {
  const changeParentDs = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [{
      name: 'parentIssueId',
      type: 'string' as FieldType,
      label: '父任务',
      textField: 'summary',
      valueField: 'issueId',
      required: true,
    }],
  }), []);

  const handleSubmit = useCallback(async () => {
    const validate = await changeParentDs.validate();
    if (validate) {
      const data = {
        issueId,
        objectVersionNumber,
        parentIssueId: changeParentDs.current?.get('parentIssueId'),
      };

      const res = await issueApi.project(projectId).subTaskChangeParent(data);
      onOk(res);
      return true;
    }
    return false;
  }, [changeParentDs, issueId, objectVersionNumber, onOk, projectId]);

  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);

  return (
    <Form dataSet={changeParentDs}>
      <SelectParentIssue name="parentIssueId" issueId={issueId} projectId={projectId} />
    </Form>
  );
};

const ObserverChangeParent = observer(ChangeParent);

const openChangeParentModal = (props: Props) => {
  Modal.open({
    key: Modal.key(),
    title: `修改${props.issueNum}的父级工作项`,
    children: <ObserverChangeParent {...props} />,
  });
};

export default openChangeParentModal;
