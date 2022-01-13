import React, { useCallback, useMemo } from 'react';
import {
  Modal, Form, TextField, Select, SelectBox, CheckBox,
} from 'choerodon-ui/pro';
import { useCreation, useDebounceFn } from 'ahooks';
import { C7NFormat } from '@choerodon/master';
import { observer } from 'mobx-react-lite';
import { MAX_LENGTH_STATUS } from '@/constants/MAX_LENGTH';
import { IStatus, IIssueType } from '@/common/types';
import StatusTypeTag from '@/components/tag/status-type-tag';
import './index.less';
import {
  statusTransformApi,
} from '@/api';
import useIsProgram from '@/hooks/useIsProgram';
import StateMachineCreateStatusProvider, { IStateMachineCreateStatusProps, useStateMachineCreateStatusContext } from './stores';

const { Option } = SelectBox;
const key = Modal.key();

const CreateStatus: React.FC = () => {
  const context = useStateMachineCreateStatusContext();
  const {
    dataSet, isOrganization, setType, setHasStatusIssueTypes, type, issueTypes, statusRecord, injectConfig,
  } = context;
  const { isProgram } = useIsProgram();
  const stageItemProps = useCreation(() => (typeof injectConfig.stageItemProps === 'function' ? injectConfig.stageItemProps(context) : {}), [context]);
  const issueTypeItemProps = useCreation(() => (injectConfig.issueTypeItemProps ? injectConfig.issueTypeItemProps(context) : {}), [context]);
  const { run: handleInputName } = useDebounceFn((value: any) => {
    if (value && String(value).trim() !== '') {
      statusTransformApi[isOrganization ? 'orgCheckStatusName' : 'checkStatusName'](value).then((data: any) => {
        const { statusExist, type: newType, existIssueTypeVO } = data;
        if (statusExist) {
          dataSet.current?.set('valueCode', newType);
          setHasStatusIssueTypes(existIssueTypeVO || []);
          setType(newType);
        } else {
          setType(null);
          setHasStatusIssueTypes([]);
        }
      });
    } else {
      setType(null);
      setHasStatusIssueTypes([]);
    }
  }, { wait: 390 });
  return (
    <Form dataSet={dataSet} className="c7n-agile-state-machine-create-status">
      <TextField
        disabled={!!statusRecord}
        name="name"
        maxLength={MAX_LENGTH_STATUS}
        valueChangeAction={'input' as any}
        placeholder="请输入状态名称，例如：测试中"
        onChange={handleInputName}
      />
      <Select
        name="valueCode"
        optionsFilter={isProgram ? undefined : (record) => (isProgram ? true : record.get('valueCode') !== 'prepare')}
        optionRenderer={({ record }) => (<StatusTypeTag code={record?.get('valueCode') as IStatus['valueCode']} />)}
        renderer={({ value }) => (value ? <StatusTypeTag code={value as IStatus['valueCode']} /> : null)}
        disabled={type !== null || !!statusRecord}
        {...stageItemProps}
      />
      <Select name="issueTypeIds" multiple disabled={!!statusRecord} {...issueTypeItemProps}>
        {(issueTypes || []).map((issueType: IIssueType) => (
          <Option value={issueType.id}>
            {issueType.name}
          </Option>
        ))}
      </Select>
      {
        !statusRecord && (
          <SelectBox name="default">
            <Option value>是</Option>
            <Option value={false}>否</Option>
          </SelectBox>
        )
      }
      <SelectBox name="completed">
        <Option value>是</Option>
        <Option value={false}>否</Option>
      </SelectBox>
      {
        !statusRecord && <CheckBox name="transferAll" />
      }
      {typeof injectConfig.extraFormItems === 'function' ? injectConfig.extraFormItems(context) : null}
    </Form>
  );
};
const ObserverCreateStatus = observer(CreateStatus);
const openCreateStatus = (props: Omit<IStateMachineCreateStatusProps, 'modal'>) => {
  Modal.open({
    title: props.record ? '编辑状态' : (
      <C7NFormat
        intlPrefix="agile.stateMachine"
        id="create.state"
      />
    ),
    key,
    drawer: true,
    style: {
      width: 380,
    },
    okText: <C7NFormat
      intlPrefix="boot"
      id={props.record ? 'save' : 'create'}
    />,
    children: (
      <StateMachineCreateStatusProvider {...props}>
        <ObserverCreateStatus />
      </StateMachineCreateStatusProvider>),
  });
};
export default openCreateStatus;
