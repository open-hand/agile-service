import React, { useMemo, useCallback, useEffect } from 'react';
import { observer } from 'mobx-react-lite';
import { Choerodon } from '@choerodon/boot';
import { DataSet, Form, Modal } from 'choerodon-ui/pro';
import { IModalProps, Issue } from '@/common/types';
import SelectParentIssue from '@/components/SelectParentIssue';
import { issueApi, statusApi } from '@/api';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import styles from './TransformSubIssue.less';
import SelectSubTask from './SelectSubTask';
import SelectSubTaskStatus from './SelectSubTaskStatus';

interface Props {
  modal?: IModalProps
  issueId: string
  projectId?:string
  objectVersionNumber: number
  onOk: (issue: Issue) => void
}
const TransformSubIssue: React.FC<Props> = ({
  modal, issueId, projectId, objectVersionNumber, onOk,
}) => {
  const transformSubIssueDs = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [{
      name: 'issueTypeId',
      label: '子任务类型',
      required: true,
    }, {
      name: 'issueId',
      label: '父任务',
      required: true,
    }, {
      name: 'statusId',
      label: '状态',
      required: true,
    }],
    events: {
      update: async ({ record, value, name }: any) => {
        if (name === 'issueTypeId' && value) {
          const firstInWorkflowStatus = await statusApi.project(projectId).loadFirstInWorkFlow(value);
          record.set('statusId', firstInWorkflowStatus);
        }
      },
    },
  }), []);

  const handleSubmit = useCallback(async () => {
    const validate = await transformSubIssueDs.validate();
    if (validate) {
      const issueTransformSubTask = {
        issueId,
        parentIssueId: transformSubIssueDs.current?.get('issueId'),
        statusId: transformSubIssueDs.current?.get('statusId'),
        objectVersionNumber,
        issueTypeId: transformSubIssueDs.current?.get('issueTypeId'),
        typeCode: 'sub_task',
      };
      return issueApi.project(projectId).taskTransformSubTask(issueTransformSubTask)
        .then((res: Issue) => {
          onOk(res);
          return true;
        }).catch(() => {
          Choerodon.prompt('转化失败');
          return false;
        });
    }
    return false;
  }, [issueId, objectVersionNumber, onOk, projectId, transformSubIssueDs]);

  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);

  return (
    <div className={styles.transformSubIssue}>
      <Form dataSet={transformSubIssueDs}>
        <SelectSubTask name="issueTypeId" projectId={projectId} />
        <SelectParentIssue name="issueId" issueId={issueId} clearButton={false} projectId={projectId} />
        <SelectSubTaskStatus name="statusId" issueTypeId={transformSubIssueDs.current?.get('issueTypeId')} projectId={projectId} />
      </Form>
    </div>
  );
};

const ObserverTransformSubIssue = observer(TransformSubIssue);

const openTransformSubIssue = (props: Props) => {
  Modal.open({
    key: Modal.key(),
    title: '转化为子工作项',
    children: <ObserverTransformSubIssue {...props} />,
    drawer: true,
    okText: '转化',
    style: {
      width: MODAL_WIDTH.small,
    },
  });
};

export default openTransformSubIssue;
