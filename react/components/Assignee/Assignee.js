import React, { useCallback, useMemo, useEffect } from 'react';
import {
  Form, Button, DataSet,
} from 'choerodon-ui/pro';
import { userApi, issueApi } from '@/api';
import { IsProjectMember } from '@/hooks/useIsProjectMember';
import SelectUser from '@/components/select/select-user';
import './Assignee.less';

const Assignee = (props) => {
  const { modal, projectId } = props;
  const dataSet = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [{
      name: 'assignee',
      label: '分配给',
      required: true,
    }],
  }), []);
  const handleClickAssignToMe = useCallback(() => {
    userApi.getSelf().then((res) => {
      if (res.id) {
        dataSet.current.set('assignee', res.id);
      }
    });
  }, [dataSet]);
  const handleSubmit = useCallback(async () => {
    if (await dataSet.validate()) {
      const assigneeId = dataSet.current.get('assignee');
      const {
        issueId, objectVersionNumber, onOk,
      } = props;
      const obj = {
        issueId,
        objectVersionNumber,
        assigneeId: assigneeId || null,
      };
      const res = await issueApi.project(projectId).update(obj);
      onOk && onOk(res);
      return true;
    }
    return false;
  }, [dataSet, props]);
  useEffect(() => {
    modal.handleOk(handleSubmit);
  }, [handleSubmit, modal]);
  return (
    <div className="c7n-agile-assignee">
      <Form style={{ width: 400 }} dataSet={dataSet}>
        <SelectUser name="assignee" projectId={projectId} />
      </Form>
      <IsProjectMember projectId={projectId}>
        {(isProjectMember) => isProjectMember && (
        <Button
          className="btn"
          funcType="raised"
          onClick={handleClickAssignToMe}
        >
          分配给我
        </Button>
        )}
      </IsProjectMember>
    </div>
  );
};
export default Assignee;
