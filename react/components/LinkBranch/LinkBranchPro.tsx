import React, { useMemo } from 'react';
import {
  Modal, Form, Select, DataSet,
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

const { Option } = Select;
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
        name: 'source', label: '服务来源', type: 'string' as FieldType, defaultValue: 'self', ignore: 'always' as FieldIgnore, required: true,
      },
      {
        name: 'app', label: '应用服务', type: 'object' as FieldType, ignore: 'always' as FieldIgnore, required: true,
      },
      {
        name: 'projectId', type: 'string' as FieldType, computedProps: { bind: ({ record }: any) => (record.get('source') === 'self' ? 'app.projectId' : 'app.value.projectId') },
      },
      {
        name: 'appServiceId', type: 'string' as FieldType, required: true, dynamicProps: { bind: ({ record }: any) => (record.get('source') === 'self' ? 'app.id' : 'app.value.id') },
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
        if (name === 'source') {
          record.init('app', undefined);
          record.init('branch', undefined);
        } else if (name === 'app') {
          record.init('branch', undefined);
        }
      },
    },
  }), []);
  const handleSubmit = async () => {
    const data = formDs.current?.toJSONData();
    if (!await formDs.validate()) {
      return false;
    }
    await devOpsApi.project(data.projectId).linkBranch(data.appServiceId, { ...data, issueIds: [issueId] }).then(() => {
      onOk && onOk();
    });
    return true;
  };
  modal?.handleOk(handleSubmit);
  return (
    <Form dataSet={formDs}>
      <Select name="source">
        <Option value="self">本项目</Option>
        <Option value="other">其他项目</Option>
      </Select>
      <SelectAppService name="app" mode={formDs.current?.get('source')} autoFocus projectId={projectId} />
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
