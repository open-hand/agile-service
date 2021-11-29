import React, { useMemo } from 'react';
import {
  Modal, Form, DataSet,
} from 'choerodon-ui/pro';
import { FieldType, FieldIgnore } from 'choerodon-ui/pro/lib/data-set/enum';
import { devOpsApi } from '@/api';
import './LinkBranch.less';
import './commom.less';
import { IModalProps } from '@/common/types';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import { observer } from 'mobx-react-lite';
import SelectAppService from '../select/select-app-service';
import SelectBranch from '../select/select-branch-with-tag';
import { getProjectId } from '@/utils/common';
import SelectBranchProject from '../select/select-branch-project';

interface ILinkBranchModalProps {
  issueId: string,
  onOk?: Function
  projectId?: string
}
const LinkBranch: React.FC<{ modal?: IModalProps } & ILinkBranchModalProps> = observer(({
  modal, projectId, issueId, onOk,
}) => {
  const formDs = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [
      {
        name: 'projectId', label: '服务来源', type: 'string' as FieldType, defaultValue: projectId || getProjectId(), required: true,
      },
      {
        name: 'appServiceId',
        label: '应用服务',
        type: 'string' as FieldType,
        required: true,
        computedProps: { disabled: ({ record }) => !record.get('projectId') },
      },
      {
        name: 'branch', label: '分支', type: 'object' as FieldType, ignore: 'always' as FieldIgnore, required: true,
      },
      {
        name: 'branchName', label: '分支', type: 'string' as FieldType, bind: 'branch.branchName', required: true,
      },
      {
        name: 'objectVersionNumber', label: '分支', type: 'string' as FieldType, bind: 'branch.objectVersionNumber',
      },

    ],
    events: {
      update: ({ record, value, name }: any) => {
        if (name === 'projectId') {
          record.init('appServiceId', undefined);
          record.init('branch', undefined);
        } else if (name === 'appServiceId') {
          record.init('branch', undefined);
        }
      },
    },
  }), [projectId]);
  const handleSubmit = async () => {
    if (!await formDs.validate()) {
      return false;
    }
    const data = formDs.current?.toJSONData();
    await devOpsApi.project(data.projectId).linkBranch(data.appServiceId, { ...data, issueIds: [issueId] }).then(() => {
      onOk && onOk();
    });
    return true;
  };
  modal?.handleOk(handleSubmit);

  return (
    <Form dataSet={formDs}>
      <SelectBranchProject name="projectId" currentProjectId={projectId || getProjectId()} />
      <SelectAppService name="appServiceId" valueField="id" mode="page" autoFocus projectId={projectId} pageTargetProjectId={formDs.current?.get('projectId')} />
      <SelectBranch name="branch" issueId={issueId} projectId={formDs.current?.get('projectId')} applicationId={formDs.current?.get('appServiceId')} enabledTag={false} />
    </Form>
  );
});
const openLinkBranchModal = (props: ILinkBranchModalProps) => {
  Modal.open({
    key: Modal.key(),
    title: '添加关联分支',
    style: {
      width: MODAL_WIDTH.small,
    },
    drawer: true,
    okText: '添加',
    children: <LinkBranch {...props} />,

  });
};
export default openLinkBranchModal;
