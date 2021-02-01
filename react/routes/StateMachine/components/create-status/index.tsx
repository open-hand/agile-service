import React, {
  useEffect, useMemo, useCallback, useState, useRef,
} from 'react';
import {
  Modal, Form, DataSet, TextField, Select, SelectBox,
} from 'choerodon-ui/pro';
import { axios } from '@choerodon/boot';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { MAX_LENGTH_STATUS } from '@/constants/MAX_LENGTH';
import { getProjectId, getOrganizationId } from '@/utils/common';
import { IStatus, IIssueType } from '@/common/types';
import StatusTypeTag from '@/components/tag/status-type-tag';
import './index.less';
import useProjectIssueTypes from '@/hooks/data/useProjectIssueTypes';
import { statusTransformApiConfig } from '@/api';
import { observer } from 'mobx-react-lite';
import useDeepCompareEffect from '@/hooks/useDeepCompareEffect';
import useIsProgram from '@/hooks/useIsProgram';

const { Option } = SelectBox;
const key = Modal.key();
interface Props {
  selectedIssueType?: string[]
  onSubmit: Function
  modal?: any
}
const CreateStatus: React.FC<Props> = ({
  modal, onSubmit, selectedIssueType = [],
}) => {
  const modalRef = useRef(modal);
  modalRef.current = modal;
  const [type, setType] = useState<IStatus['valueCode'] | null>(null);
  const { data: issueTypes } = useProjectIssueTypes();
  // 记录哪些类型下已经有同名状态
  const [hasStatusIssueTypes, setHasStatusIssueTypes] = useState<IIssueType[]>([]);
  const hasStatusIssueTypesRef = useRef<IIssueType[]>([]);
  hasStatusIssueTypesRef.current = hasStatusIssueTypes;
  const { isProgram } = useIsProgram();
  const dataSet = useMemo(() => new DataSet({
    autoCreate: true,
    transport: {
      create: ({ data: dataArray }) => {
        const data = dataArray[0];
        return statusTransformApiConfig.createStatus(data.issueTypeIds, {
          name: data.name,
          type: data.valueCode,
          defaultStatus: data.default,
          transferAll: data.transferAll,
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
        label: '阶段',
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
        label: '问题类型',
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
    ],
  }), []);
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
    if (await dataSet.validate()) {
      try {
        await dataSet.submit();
        onSubmit();
        return true;
      } catch (error) {
        return false;
      }
    }
    return false;
  }, [dataSet, onSubmit]);
  useEffect(() => {
    modal.handleOk(handleSubmit);
  }, [modal, handleSubmit]);

  return (
    <>
      <Form dataSet={dataSet}>
        <TextField
          name="name"
          maxLength={MAX_LENGTH_STATUS}
          onInput={(e) => {
            // @ts-ignore
            const { value } = e.target;
            if (value) {
              axios({
                url: `agile/v1/projects/${getProjectId()}/status/project_check_name`,
                method: 'GET',
                params: {
                  organization_id: getOrganizationId(),
                  name: value,
                },
                data: null,
              }).then((data: any) => {
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
          disabled={type !== null}
        />
        <Select name="issueTypeIds" multiple>
          {(issueTypes || []).map((issueType) => (
            <Option value={issueType.id}>
              {issueType.name}
            </Option>
          ))}
        </Select>
        <SelectBox name="default" style={{ marginTop: 13 }}>
          <Option value>是</Option>
          <Option value={false}>否</Option>
        </SelectBox>
        <SelectBox name="transferAll">
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
    title: '创建状态',
    key,
    drawer: true,
    style: {
      width: 380,
    },
    children: <ObserverCreateStatus {...props} />,
  });
};
export default openCreateStatus;
