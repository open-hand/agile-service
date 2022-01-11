import React from 'react';
import {
  Modal, Form, TextField, Select, SelectBox, CheckBox,
} from 'choerodon-ui/pro';
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
  const {
    dataSet, isOrganization, setType, setHasStatusIssueTypes, type, issueTypes, statusRecord, injectConfig,
  } = useStateMachineCreateStatusContext();
  const { isProgram } = useIsProgram();

  return (
    <Form dataSet={dataSet}>
      <TextField
        disabled={!!statusRecord}
        name="name"
        maxLength={MAX_LENGTH_STATUS}
        valueChangeAction={'input' as any}
        placeholder="请输入状态名称，例如：测试中"
        onInput={(e) => {
          // @ts-ignore
          const { value } = e.target;
          if (value) {
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
        }}
      />
      <Select
        name="valueCode"
        optionsFilter={isProgram ? undefined : (record) => (isProgram ? true : record.get('valueCode') !== 'prepare')}
        optionRenderer={({ record }) => (<StatusTypeTag code={record?.get('valueCode') as IStatus['valueCode']} />)}
        renderer={({ value }) => (value ? <StatusTypeTag code={value as IStatus['valueCode']} /> : null)}
        disabled={type !== null || !!statusRecord}
      />
      <Select name="issueTypeIds" multiple disabled={!!statusRecord}>
        {(issueTypes || []).map((issueType: IIssueType) => (
          <Option value={issueType.id}>
            {issueType.name}
          </Option>
        ))}
      </Select>
      {
          !statusRecord && (
            <SelectBox name="default" style={{ marginTop: 13 }}>
              <Option value>是</Option>
              <Option value={false}>否</Option>
            </SelectBox>
          )
        }
      <SelectBox name="completed" style={{ marginTop: statusRecord ? 13 : 0 }}>
        <Option value>是</Option>
        <Option value={false}>否</Option>
      </SelectBox>
      {
          !statusRecord && <CheckBox name="transferAll" />
        }
      {injectConfig.extraFormItems}
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
