import React, {
  useState, useEffect, useCallback, useMemo,
} from 'react';
import {
  Modal, Form, Select, DataSet,
} from 'choerodon-ui/pro';
import { Choerodon } from '@choerodon/boot';
import { observer } from 'mobx-react-lite';
import { IModalProps } from '@/common/types';
import EditIssueStore from '@/components/EditIssue/stores/EditIssueStore';
import { uiApi } from '@/api';
import styles from './Link.less';
import { IUi } from '../delete/DeleteUI';

interface Props {
  modal?: IModalProps,
  store: EditIssueStore
  reloadIssue: Function
}

const Link: React.FC<Props> = ({ modal, store, reloadIssue }) => {
  const { issue } = store;
  const optionDataSet: DataSet = useMemo(() => new DataSet({
    fields: [{
      name: 'ui',
      textField: 'id',
      valueField: 'id',
      required: true,
      options: optionDataSet,
      multiple: true,
    }],

  }), []);

  useEffect(() => {
    // @ts-ignore
    if (issue.issueId) {
      // @ts-ignore
      uiApi.project(issue?.projectId).getUIUnLinked(issue.issueId).then((res: IUi[]) => {
        optionDataSet.loadData(res);
      });
    }
    // @ts-ignore
  }, [issue.issueId, optionDataSet]);

  const linkedDataSet = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [{
      name: 'ui',
      textField: 'fileName',
      valueField: 'id',
      required: true,
      options: optionDataSet,
      multiple: true,
      label: '选择已有文件',
    }],
  }), [optionDataSet]);

  const handleSubmit = useCallback(async () => {
    const validate = await linkedDataSet.validate();
    if (validate) {
      const linkedUI = linkedDataSet.current?.get('ui');
      // @ts-ignore
      if (linkedUI && linkedUI.length && issue?.issueId) {
        uiApi.project(issue?.projectId).linkUI({
          // @ts-ignore
          issueId: issue.issueId,
          fileHeaderIds: linkedUI,
        }).then(() => {
          modal?.close();
          store.getLinkedUI();
          reloadIssue();
          Choerodon.prompt('关联成功');
          return true;
        }).catch(() => {
          Choerodon.prompt('关联失败');
        });
      }
    }
    return false;
    // @ts-ignore
  }, [issue.issueId, linkedDataSet, modal, reloadIssue, store]);
  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);

  return (
    <Form dataSet={linkedDataSet}>
      <Select name="ui" />
    </Form>
  );
};

const ObserverLink = observer(Link);

const openLinkUI = (props: Props) => {
  Modal.open({
    key: 'linkUI/UX',
    title: '关联UI/UX文件',
    drawer: true,
    style: {
      width: 380,
    },
    className: styles.linkUIModal,
    children: <ObserverLink {...props} />,
  });
};

export default openLinkUI;
