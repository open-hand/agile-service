import React, { useEffect, useMemo } from 'react';
import {
  Form, TextField, Modal, TextArea, DataSet,
} from 'choerodon-ui/pro';
import { stores, Choerodon } from '@choerodon/boot';
import { issueApi, fieldApi, epicApi } from '@/api';
import { checkCanQuickCreate } from '@/utils/quickCreate';
import BacklogStore from '../../../../stores/project/backlog/BacklogStore';

const { AppState } = stores;

const CreateEpic = (props) => {
  const { modal } = props;
  const checkEpicNameRepeat = async (value) => {
    const res = await epicApi.checkName(value);
    if (res) {
      return '史诗名称重复';
    }
    return true;
  };
  const dataSet = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [{
      name: 'name',
      required: true,
      validator: checkEpicNameRepeat,
      label: '史诗名称',
    }, {
      name: 'summary',
      label: '概要',
      required: true,
    }],
  }), []);
  /**
   *
   * 创建史诗
   * @param {*} e
   * @memberof CreateEpic
   */
  const handleCreateEpic = async () => {
    const {
      refresh, cantCreateEvent, summaryChange, typeIdChange, epicNameChange,
    } = props;
    const issueTypes = BacklogStore.getIssueTypes || [];
    const defaultPriorityId = BacklogStore.getDefaultPriority ? BacklogStore.getDefaultPriority.id : '';
    try {
      if (!await dataSet.validate()) {
        return false;
      }
      const value = dataSet.toData()[0];
      const epicType = issueTypes.find((t) => t.typeCode === 'issue_epic');
      const req = {
        projectId: AppState.currentMenuType.id,
        epicName: value.name.trim(),
        summary: value.summary.trim(),
        typeCode: 'issue_epic',
        issueTypeId: epicType && epicType.id,
        priorityCode: `priority-${defaultPriorityId}`,
        priorityId: defaultPriorityId,
      };
      if (!await checkCanQuickCreate(epicType.id)) {
        if (!cantCreateEvent) {
          Choerodon.prompt('该工作项类型含有必填选项，请使用创建工作项弹框创建');
        } else {
          Choerodon.prompt('请填写标注的必填字段');
          if (summaryChange) {
            summaryChange(value.summary.trim());
          }
          if (typeIdChange) {
            typeIdChange(epicType && epicType.id);
          }
          if (epicNameChange) {
            epicNameChange(value.name.trim());
          }
          cantCreateEvent();
        }
        return true;
      }
      const res = await issueApi.create(req);
      const dto = {
        schemeCode: 'agile_issue',
        issueTypeId: res.issueTypeId,
        pageCode: 'agile_issue_create',
      };
      fieldApi.quickCreateDefault(res.issueId, dto);
      refresh();
      return true;
    } catch (error) {
      Choerodon.prompt(error.message, 'error');
      return false;
    }
  };

  useEffect(() => {
    modal.handleOk(handleCreateEpic);
  }, []);

  return (
    <Form dataSet={dataSet}>
      <TextField name="name" maxLength={20} valueChangeAction="input" showLengthInfo />
      <TextArea name="summary" maxLength={44} valueChangeAction="input" showLengthInfo />
    </Form>
  );
};

export default function openCreateEpic(props) {
  Modal.open({
    key: 'create-epic',
    title: '创建史诗',
    okText: '创建',
    cancelText: '取消',
    drawer: true,
    style: {
      width: 380,
    },
    children: <CreateEpic {...props} />,
  });
}
