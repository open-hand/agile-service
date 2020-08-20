import React, {
  useEffect, useMemo, useCallback, useState,
} from 'react';
import {
  Modal, Form, DataSet, TextField, Select, SelectBox,
} from 'choerodon-ui/pro';
import { stores } from '@choerodon/boot';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { MAX_LENGTH_STATUS } from '@/constants/MAX_LENGTH';
import { getProjectId, getOrganizationId } from '@/utils/common';
import { IStatus } from '@/common/types';
import StatusTypeTag from '@/components/tag/status-type-tag';

import './index.less';
import { useIssueTypes } from '@/hooks';
import { statusTransformApiConfig } from '@/api';

const { AppState } = stores;
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
  const [type, setType] = useState<IStatus['valueCode'] | null>(null);
  const [issueTypes] = useIssueTypes();
  const isProgram = AppState.currentMenuType.category === 'PROGRAM';
  const dataSet = useMemo(() => new DataSet({
    autoCreate: true,
    transport: {
      create: ({ data: dataArray }) => {
        const data = dataArray[0];
        return statusTransformApiConfig.createStatus(data.issueTypeIds, {
          name: data.name,
          type: data.valueCode,
          defaultStatus: data.default,
        });
      },
      // 进行验证 当存在同名状态 将类别自动选择
      validate: ({ data: { unique } }) => ({
        url: `agile/v1/projects/${getProjectId()}/status/project_check_name`,
        method: 'GET',
        params: {
          organization_id: getOrganizationId(),
          name: unique[0].name,
        },
        data: null,
        transformResponse: (res) => {
          const data = JSON.parse(res);
          const { statusExist, type: newType } = data;
          if (statusExist) {
            dataSet.current?.set('valueCode', newType);
            setType(newType);
          } else {
            setType(null);
          }
          return true;
        },
      }),
    },
    fields: [
      {
        name: 'name',
        type: 'string' as FieldType,
        label: '状态名称',
        required: true,
        unique: true,
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
        multiple: true,
      },
      {
        name: 'default',
        type: 'boolean' as FieldType,
        defaultValue: false,
        label: '是否设置为初始状态?',
        required: true,
      },
    ],
  }), []);
  useEffect(() => {
    if (selectedIssueType?.length > 0) {
      dataSet.current?.set('issueTypeIds', selectedIssueType);
    }
  }, [selectedIssueType]);
  useEffect(() => {
    if (type && type !== null) {
      dataSet.current?.set('categoryCode', type);
    }
  }, [type]);
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
        <TextField name="name" maxLength={MAX_LENGTH_STATUS} />
        <Select
          name="valueCode"
          optionsFilter={isProgram ? undefined : (record) => (isProgram ? true : record.get('valueCode') !== 'prepare')}
          optionRenderer={({ record }) => (<StatusTypeTag code={record?.get('valueCode') as IStatus['valueCode']} />)}
          disabled={type !== null}
        />
        <Select name="issueTypeIds">
          {issueTypes.map((issueType) => (
            <Option value={issueType.id}>
              {issueType.name}
            </Option>
          ))}
        </Select>
        <SelectBox name="default">
          <Option value>是</Option>
          <Option value={false}>否</Option>
        </SelectBox>
      </Form>
    </>
  );
};
const openCreateStatus = (props: Omit<Props, 'modal'>) => {
  Modal.open({
    title: '创建状态',
    key,
    drawer: true,
    style: {
      width: 380,
    },

    children: <CreateStatus {...props} />,
  });
};
export default openCreateStatus;
