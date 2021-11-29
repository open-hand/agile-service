import React, { useMemo, useCallback } from 'react';
import {
  Modal, Form, Select, DataSet, Row, Col, TextField,
} from 'choerodon-ui/pro';
import { FieldType, FieldIgnore } from 'choerodon-ui/pro/lib/data-set/enum';
import { observer } from 'mobx-react-lite';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { devOpsApi, projectApi } from '@/api';
import { IModalProps } from '@/common/types';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import { getProjectId, getProjectName } from '@/utils/common';
import SelectAppService from '../select/select-app-service';
import SelectBranch from '../select/select-branch-with-tag';
import styles from './CreateBranchPro.less';
import { colorTransformRgba } from './utils';
import SelectProject from '../select/select-project';
import SelectBranchProject from '../select/select-branch-project';

const { Option } = Select;
interface ILinkBranchModalProps {
  issueId: string
  typeCode?: string
  projectId?: string
  defaultBranchSuffixName?: string
  onOk?: Function
}

const SelectBranchType: React.FC<{ name: string }> = ({ name }) => {
  const optionData = useMemo(() => [
    { name: 'feature', color: '#f953ba' },
    { name: 'bugfix', color: '#ffb100' },
    { name: 'release', color: '#1bc123' },
    { name: 'hotfix', color: '#f44336' },
    { name: 'custom', color: '#af4cff' },
  ], []);
  return (
    <Select name={name} style={{ width: '100%' }} className={styles.branchType}>
      {optionData.map((item) => (
        <Option value={item.name} key={item.name}>
          <span className={styles.branchType_icon} style={{ color: item.color, backgroundColor: colorTransformRgba(item.color, 0.2) }}>
            {item.name.slice(0, 1).toUpperCase()}
          </span>
          <span>{item.name}</span>
        </Option>
      ))}
    </Select>
  );
};

const CreateBranch: React.FC<{ modal?: IModalProps } & ILinkBranchModalProps> = observer(({
  modal, issueId, typeCode, onOk, defaultBranchSuffixName, projectId: propsProjectId,
}) => {
  const handleCheckName = useCallback(async (value?: string, name?: string, record?: Record) => {
    const endWith = /(\/|\.|\.lock)$/;
    const contain = /(\s|~|\^|:|\?|\*|\[|\\|\.\.|@\{|\/{2,}){1}/;
    const single = /^@+$/;
    if (!value) {
      return false;
    }
    if (endWith.test(value)) {
      return '不能以"/"、"."、".lock"结尾';
    }
    if (contain.test(value) || single.test(value)) {
      return '只能包含字母、数字、\'——\'、\'_\'';
    }
    const appServiceId = record?.get('appServiceId');
    const projectId = record?.get('projectId') || getProjectId();
    if (appServiceId && projectId) {
      const branchType = record?.get('branchType');
      const branchName = branchType === 'custom' ? record?.get('branchName') : `${branchType}-${record?.get('branchName')}`;
      const res = await devOpsApi.project(projectId).checkBranchName(appServiceId, branchName);
      return !res ? '分支名称已存在' : true;
    }

    return true;
  }, []);
  const formDs = useMemo(() => {
    const typeMapBranchType = {
      bug: 'bugfix',
      task: 'feature',
      story: 'feature',
      issue_epic: 'feature',
      sub_task: 'feature',
    };
    const typeDefaultValue = typeMapBranchType[typeCode as keyof typeof typeMapBranchType] || 'feature';
    return new DataSet({
      autoCreate: true,
      fields: [
        {
          name: 'projectId', label: '服务来源', type: 'string' as FieldType, defaultValue: propsProjectId ?? getProjectId(), required: true,
        },
        {
          name: 'appServiceId',
          label: '应用服务',
          type: 'string' as FieldType,
          required: true,
          computedProps: { disabled: ({ record }) => !record.get('projectId') },

        },
        {
          name: 'originBranch', label: '分支来源', type: 'string' as FieldType, required: true,
        },
        {
          name: 'branchType', label: '分支类型', type: 'string' as FieldType, required: true, defaultValue: typeDefaultValue,
        },
        {
          name: 'branchName', label: '分支名称', type: 'string' as FieldType, required: true, validator: handleCheckName, defaultValue: defaultBranchSuffixName,
        },

      ],
      events: {
        update: ({ record, value, name }: any) => {
          if (name === 'projectId') {
            record.init('appServiceId', undefined);
            record.init('originBranch', undefined);
          } else if (name === 'appServiceId') {
            record.init('originBranch', undefined);
          }
        },
      },
    });
  }, [defaultBranchSuffixName, handleCheckName, propsProjectId, typeCode]);
  const handleSubmit = async () => {
    if (!await formDs.validate()) {
      return false;
    }
    const data = formDs.current?.toJSONData();
    const devopsBranchVO = {
      branchName: data.branchType === 'custom' ? data.branchName : `${data.branchType}-${data.branchName}`,
      issueId,
      originBranch: data.originBranch,
    };
    await devOpsApi.project(data.projectId).createBranch(data.appServiceId, devopsBranchVO).then(() => {
      onOk && onOk();
    });
    return true;
  };
  modal?.handleOk(handleSubmit);

  return (
    <Form dataSet={formDs}>
      <SelectBranchProject name="projectId" currentProjectId={propsProjectId || getProjectId()} />
      <SelectAppService name="appServiceId" valueField="id" mode="page" autoFocus projectId={propsProjectId} pageTargetProjectId={formDs.current?.get('projectId')} />
      <SelectBranch name="originBranch" issueId={issueId} projectId={formDs.current?.get('projectId')} applicationId={formDs.current?.get('appServiceId')} enabledTag />
      <Row>
        <Col span={9} style={{ paddingRight: '.2rem' }}>
          <SelectBranchType name="branchType" />
        </Col>
        <Col span={15}>
          <TextField name="branchName" colSpan={6} style={{ width: '100%' }} />
        </Col>
      </Row>
    </Form>
  );
});
const openCreateBranchModal = (props: ILinkBranchModalProps) => {
  Modal.open({
    key: Modal.key(),
    title: '创建分支',
    style: {
      width: MODAL_WIDTH.small,
    },
    drawer: true,
    okText: '创建',
    children: <CreateBranch {...props} />,

  });
};
export default openCreateBranchModal;
