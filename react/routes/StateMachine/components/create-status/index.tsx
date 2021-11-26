import React, {
  useEffect, useMemo, useCallback, useState, useRef,
} from 'react';
import {
  Modal, Form, DataSet, TextField, Select, SelectBox,
} from 'choerodon-ui/pro';
import { C7NFormat } from '@choerodon/master';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { observer } from 'mobx-react-lite';
import { MAX_LENGTH_STATUS } from '@/constants/MAX_LENGTH';
import { getProjectId, getIsOrganization } from '@/utils/common';
import { IStatus, IIssueType } from '@/common/types';
import StatusTypeTag from '@/components/tag/status-type-tag';
import './index.less';
import useIssueTypes from '@/hooks/data/useIssueTypes';
import {
  boardApiConfig, statusTransformApi, statusTransformApiConfig,
} from '@/api';
import useDeepCompareEffect from '@/hooks/useDeepCompareEffect';
import useIsProgram from '@/hooks/useIsProgram';
import { OldLoading as Loading } from '@/components/Loading';
import useFormatMessage from '@/hooks/useFormatMessage';

const { Option } = SelectBox;
const key = Modal.key();
interface Props {
  selectedIssueType?: string[]
  onSubmit: Function
  modal?: any
  record?: Record
}
const CreateStatus: React.FC<Props> = ({
  modal, onSubmit, selectedIssueType = [], record: statusRecord,
}) => {
  const formatMessage = useFormatMessage();

  const [loading, setLoading] = useState<boolean>(false);
  const modalRef = useRef(modal);
  modalRef.current = modal;
  const isOrganization = getIsOrganization();
  const [type, setType] = useState<IStatus['valueCode'] | null>(null);
  const [editStatus, setEditStatus] = useState<any>(null);
  const { data: issueTypes } = useIssueTypes({ hasTemplate: true, excludeTypes: ['feature', 'issue_auto_test', 'issue_test', ...(isOrganization ? ['issue_epic'] : [])] });
  // 记录哪些类型下已经有同名状态
  const [hasStatusIssueTypes, setHasStatusIssueTypes] = useState<IIssueType[]>([]);
  const hasStatusIssueTypesRef = useRef<IIssueType[]>([]);
  hasStatusIssueTypesRef.current = hasStatusIssueTypes;
  const { isProgram } = useIsProgram();
  const dataSet = useMemo(() => new DataSet({
    autoCreate: true,
    transport: {
      submit: ({ data: dataArray }) => {
        const data = dataArray[0];
        return statusRecord ? boardApiConfig.updateStatus(editStatus?.issueStatusId, {
          objectVersionNumber: editStatus?.issueStatusObjectVersionNumberId,
          completed: data.completed,
          statusId: editStatus?.id,
          id: editStatus?.issueStatusId,
          projectId: getProjectId(),
        }) : statusTransformApiConfig[isOrganization ? 'orgCreateStatus' : 'createStatus'](data.issueTypeIds, {
          name: data.name,
          type: data.valueCode,
          defaultStatus: data.default,
          transferAll: data.transferAll,
          completed: data.completed,
        });
      },
    },
    fields: [
      {
        name: 'name',
        type: 'string' as FieldType,
        label: '状态名称',
        required: true,
      },
      {
        name: 'valueCode',
        type: 'string' as FieldType,
        label: formatMessage({ id: 'agile.stateMachine.stage' }),
        required: true,
        lookupAxiosConfig: () => ({
          url: '/agile/v1/lookup_values/status_category',
          transformResponse: (data) => (Array.isArray(data) ? data : JSON.parse(data).lookupValues),
        }),
        textField: 'name',
        valueField: 'valueCode',
      },
      {
        name: 'issueTypeIds',
        type: 'string' as FieldType,
        label: formatMessage({ id: 'agile.common.issueType' }),
        required: true,
        textField: 'name',
        valueField: 'id',
        validator: async (value) => {
          const result = hasStatusIssueTypesRef.current.filter((item) => value && value.some((v: string) => v === item.id));
          if (result.length > 0) {
            return `${result.map((i) => i.name).join(',')}下已有同名状态`;
          }
          return true;
        },
      },
      {
        name: 'default',
        type: 'boolean' as FieldType,
        defaultValue: false,
        label: '是否设置为初始状态?',
        required: true,
      },
      {
        name: 'transferAll',
        type: 'boolean' as FieldType,
        defaultValue: true,
        label: '是否转换到所有状态?',
        required: true,
      },
      {
        name: 'completed',
        type: 'boolean' as FieldType,
        defaultValue: false,
        label: '是否为已解决状态？',
        required: true,
      },
    ],
  }), [editStatus?.id, editStatus?.issueStatusId, editStatus?.issueStatusObjectVersionNumberId, isOrganization, statusRecord]);
  useEffect(() => {
    if (selectedIssueType?.length > 0) {
      dataSet.current?.set('issueTypeIds', selectedIssueType);
    }
  }, [dataSet, selectedIssueType]);
  useEffect(() => {
    if (type && type !== null) {
      dataSet.current?.set('categoryCode', type);
    }
  }, [dataSet, type]);
  const issueTypeIds = dataSet.current?.get('issueTypeIds');
  const disableCreate = useMemo(() => hasStatusIssueTypes.find((issueType) => (issueTypeIds || []).find((id: string) => id === issueType.id)), [hasStatusIssueTypes, issueTypeIds]);
  useDeepCompareEffect(() => {
    modalRef.current.update({
      okProps: {
        disabled: disableCreate,
      },
    });
  }, [disableCreate]);
  useDeepCompareEffect(() => {
    if ((dataSet.current?.get('issueTypeIds') || []).length > 0) {
      dataSet.current?.getField('issueTypeIds')?.checkValidity();
    }
  }, [hasStatusIssueTypes]);
  const handleSubmit = useCallback(async () => {
    if (await dataSet.submit()) {
      onSubmit();
      return true;
    }
    return false;
  }, [dataSet, onSubmit]);
  useEffect(() => {
    modal.handleOk(handleSubmit);
  }, [modal, handleSubmit]);

  useEffect(() => {
    if (statusRecord) {
      setLoading(true);
      statusTransformApi.getStatus(statusRecord.get('id')).then((res: any) => {
        setEditStatus(res);
        setLoading(false);
        dataSet.current?.set('name', res.name);
        dataSet.current?.set('valueCode', res.type);
        dataSet.current?.set('issueTypeIds', res.issueTypeIds);
        dataSet.current?.set('completed', res.completed);
      });
    }
  }, [dataSet, statusRecord]);

  return (
    <>
      <Loading loading={loading} />
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
        {
          !statusRecord && (
            <SelectBox name="transferAll">
              <Option value>是</Option>
              <Option value={false}>否</Option>
            </SelectBox>
          )
        }
        <SelectBox name="completed" style={{ marginTop: statusRecord ? 13 : 0 }}>
          <Option value>是</Option>
          <Option value={false}>否</Option>
        </SelectBox>
      </Form>
    </>
  );
};
const ObserverCreateStatus = observer(CreateStatus);
const openCreateStatus = (props: Omit<Props, 'modal'>) => {
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
      intlPrefix="agile.stateMachine"
      id={props.record ? 'boot.save' : 'boot.create'}
    />,
    children: <ObserverCreateStatus {...props} />,
  });
};
export default openCreateStatus;
