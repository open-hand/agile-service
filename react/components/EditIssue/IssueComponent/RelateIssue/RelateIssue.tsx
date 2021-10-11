import React, {
  useCallback, useEffect, useMemo, useState,
} from 'react';
import { observer } from 'mobx-react-lite';
import {
  Spin, Modal, DataSet, Form,
} from 'choerodon-ui/pro';

import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { IModalProps, Issue } from '@/common/types';
import { issueApi, issueTypeApi } from '@/api';
import { map } from 'lodash';
import SelectRelateIssue from './SelectRelateIssue';

interface Props {
  issue: Issue
  modal?: IModalProps
  onOk: (issue: Issue) => void
}

const RelateIssue: React.FC<Props> = ({
  issue, modal, onOk,
}) => {
  const [loading, setLoading] = useState<boolean>(false);
  const [issueTypeId, setIssueTypeId] = useState<string[]>([]);
  const [relateIssue, setRelateIssue] = useState<undefined | Issue>();
  const relateIssueDs = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [{
      name: 'relateIssueId',
      type: 'string' as FieldType,
      label: '工作项',
      textField: 'summary',
      valueField: 'issueId',
    }],
  }), []);

  const handleSubmit = useCallback(async () => {
    const validate = await relateIssueDs.validate();
    if (validate) {
      const { issueId, objectVersionNumber } = issue;
      const data = {
        issueId,
        objectVersionNumber,
        relateIssueId: relateIssueDs.current?.get('relateIssueId') || 0,
      };

      const res = await issueApi.update(data);
      onOk(res);
      return true;
    }
    return false;
  }, [relateIssueDs, issue, onOk]);

  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);

  useEffect(() => {
    setLoading(true);
    issueTypeApi.loadAllWithStateMachineId().then((issueTypes) => {
      setLoading(false);
      const types = issueTypes.filter((type) => type.typeCode === 'story' || type.typeCode === 'task');
      if (types.length) {
        setIssueTypeId(map(types, 'id'));
      }
    });
  }, []);

  useEffect(() => {
    if (issue.relateIssueId) {
      relateIssueDs.current?.set('relateIssueId', issue.relateIssueId);
    }
  }, [issue.relateIssueId, relateIssueDs]);

  useEffect(() => {
    if (issue.relateIssueId) {
      issueApi.load(issue.relateIssueId).then((task: undefined | Issue) => {
        if (task) {
          setRelateIssue(task);
        }
      });
    }
  }, [issue.relateIssueId]);

  return (
    <Spin spinning={loading}>
      <Form dataSet={relateIssueDs}>
        {
          issueTypeId?.length && (
            <SelectRelateIssue name="relateIssueId" issueTypeId={issueTypeId} relateIssue={relateIssue} />
          )
        }
      </Form>
    </Spin>
  );
};

const ObserverRelateIssue = observer(RelateIssue);

const openRelateIssueModal = (props: Props) => {
  Modal.open({
    key: Modal.key(),
    title: '关联工作项',
    children: <ObserverRelateIssue {...props} />,
    drawer: true,
  });
};

export default openRelateIssueModal;
