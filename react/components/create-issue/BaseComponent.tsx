import React, {
  useEffect, useMemo, useRef, useState,
} from 'react';
import {
  DataSet, Row, Col, Spin, Form,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { usePersistFn } from 'ahooks';
import { find, merge } from 'lodash';
import { UploadFile } from 'choerodon-ui/lib/upload/interface';
import UploadButton from '@/components/CommonComponent/UploadButton';
import validateFile from '@/utils/File';
import useProjectIssueTypes from '@/hooks/data/useProjectIssueTypes';
import { IIssueType, IModalProps, IssueCreateFields } from '@/common/types';
import useIssueCreateFields from '@/hooks/data/useIssueCreateFields';
import moment from 'moment';
import getFieldConfig from './fields';
import { insertField } from './utils';

export interface CreateIssueBaseProps {
  onSubmit: ({ data, fieldList }: {
    data: {
      [key: string]: any
    }
    fieldList: {
      fieldType: string,
      value: any,
      fieldId: string,
      fieldCode: string,
    }[]
    fileList: UploadFile[]
  }) => void,
  modal?: IModalProps,
  projectId?: string,
  defaultTypeCode?: string
  defaultTypeId?: string
}
const defaultDataSet = new DataSet({
  autoCreate: true,
  fields: [],
});
const presets = new Map([
  ['component', {
    type: 'object',
    valueField: 'componentId',
  }],
  ['label', {
    type: 'object',
    valueField: 'labelId',
  }],
  ['estimatedStartTime', {
    max: 'estimatedEndTime',
  }],
  ['estimatedEndTime', {
    min: 'estimatedStartTime',
  }],
]);
const lineField = ['summary', 'description'];
const reuseFields = ['issueType', 'summary', 'description'];
function getDefaultValue(field: IssueCreateFields) {
  const preset = presets.get(field.fieldCode);
  if (preset) {
    if (preset.type === 'object') {
      const isMultiple = field.fieldType === 'multiple' || field.fieldType === 'checkbox';
      return isMultiple ? field.defaultValueObjs : field.defaultValueObj;
    }
  }
  return field.defaultValue;
}
function transformSubmitFieldValue(field: IssueCreateFields, value: any) {
  switch (field.fieldType) {
    case 'time':
    case 'date':
    case 'datetime': {
      return value && moment.isMoment(value) ? value.format('YYYY-MM-DD HH:mm:ss') : value;
    }
    default: return value;
  }
}
const CreateIssueBase = observer(({
  modal, projectId, onSubmit, defaultTypeCode = 'story', defaultTypeId,
}: CreateIssueBaseProps) => {
  const [fileList, setFileList] = useState<UploadFile[]>([]);
  const dataSetRef = useRef(defaultDataSet);
  const [dataSet, setDataSet] = useState(defaultDataSet);
  dataSetRef.current = dataSet;
  const setFieldValue = usePersistFn((name, value) => {
    dataSet.current?.set(name, value);
  });
  const { isFetching: isLoading, data: issueTypeList } = useProjectIssueTypes({

  }, {
    onSuccess: ((issueTypes) => {
      setFieldValue('issueType', getDefaultIssueType(issueTypes));
    }),
  });
  const getDefaultIssueType = (issueTypes: IIssueType[]) => {
    if (defaultTypeId && find(issueTypes, { id: defaultTypeId })) {
      return find(issueTypes, { id: defaultTypeId })?.id;
    }
    if (defaultTypeCode && find(issueTypes, { typeCode: defaultTypeCode })) {
      return find(issueTypes, { typeCode: defaultTypeCode })?.id;
    }
    if (issueTypes && issueTypes.length > 0) {
      return issueTypes[0].id;
    }
    return undefined;
  };
  const issueTypeId = dataSet.current && dataSet.current.get('issueType');
  const issueTypeCode = find(issueTypeList, {
    id: issueTypeId,
  })?.typeCode;
  const isSubIssue = issueTypeCode && ['sub_task'].includes(issueTypeCode);
  const handleUpdate = usePersistFn(({ name, value }) => {
    switch (name) {
      case 'issueType': {
        break;
      }
      default: break;
    }
  });
  const [{ data: fields, isFetching: isFieldsLoading }, {
    data: templateData,
  }] = useIssueCreateFields({ issueTypeId });
  useEffect(() => {
    const oldDataSet = dataSetRef.current;
    const newDataSet = new DataSet({
      autoCreate: false,
      fields: fields ? insertField([...fields.map((field) => {
        const preset = presets.get(field.fieldCode) ?? {};
        return merge(preset, {
          name: field.fieldCode,
          fieldId: field.fieldId,
          fieldType: field.fieldType,
          fieldCode: field.fieldCode,
          label: field.fieldName,
          required: field.required,
        });
      })], [{
        insert: !!isSubIssue,
        after: 'issueType',
        field: {
          name: 'parentIssueId',
          label: '关联父级任务',
          required: true,
        },
      }]) : [],
    });

    const newValue: { [key: string]: any } = {};
    // 优先保留之前的值
    reuseFields.forEach((name) => {
      const oldValue = oldDataSet.current?.get(name);
      if (oldValue) {
        newValue[name] = oldValue;
      }
    });
    const setValue = (name: string, value: any) => {
      if (newValue[name] === null || newValue[name] === undefined) {
        newValue[name] = value;
      }
    };

    // 设置默认值
    fields?.forEach((field) => {
      const defaultValue = getDefaultValue(field);
      if (defaultValue !== null && defaultValue !== undefined) {
        // 没有值的时候再设置
        setValue(field.fieldCode, defaultValue);
      }
    });
    // 设置描述默认值
    if (templateData && templateData.template) {
      setValue('description', templateData.template);
    }
    // 创建一个新的
    newDataSet.create(newValue);
    setDataSet(newDataSet);
  }, [fields, isSubIssue, templateData]);
  const handleSubmit = usePersistFn(async () => {
    if (await dataSet.validate()) {
      const data = dataSet.current?.toData();
      const customFields = fields?.filter((f) => !f.system);
      const systemFields = fields?.filter((f) => f.system);
      const fieldList = customFields?.map((field) => ({
        fieldType: field.fieldType,
        value: transformSubmitFieldValue(field, data[field.fieldCode]),
        fieldId: field.fieldId,
        fieldCode: field.fieldCode,
      })) ?? [];
      const values = systemFields?.reduce((res, field) => {
        const config = getFieldConfig(field);
        return {
          ...res,
          ...{
            [config.valueKey ?? field.fieldCode]: data[field.fieldCode],
          },
        };
      }, {}) ?? {};
      const issueType = find(issueTypeList, {
        id: data.issueType,
      });
      Object.assign(values, {
        typeCode: (issueType as IIssueType)?.typeCode,
        priorityCode: `priority-${data.priority || 0}`,
        parentIssueId: data.parentIssueId,
        versionIssueRelVOList: [
          ...(data.fixVersion ?? []).map((versionId: string) => ({ versionId, relationType: 'fix' })),
          ...(data.influenceVersion ?? []).map((versionId: string) => ({ versionId, relationType: 'influence' })),
        ],
        fixVersion: undefined,
        influenceVersion: undefined,
      });
      await onSubmit({
        data: values, fieldList, fileList,
      });
      return true;
    }
    return false;
  });
  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);
  const getFieldProps = usePersistFn((field?: IssueCreateFields): { [key: string]: any } => {
    if (!field) {
      return {};
    }
    switch (field.fieldCode) {
      case 'parentIssueId': {
        return {
          issueType: issueTypeCode,
        };
      }
      default: break;
    }
    switch (field.fieldType) {
      case 'member': {
        return {
          extraOptions: field.defaultValueObj,
        };
      }
      case 'multiMember': {
        return {
          extraOptions: field.defaultValueObjs,
        };
      }
      default: return {};
    }
  });
  const renderFields = usePersistFn(() => (
    <Row gutter={24}>
      {[...dataSet.fields.values()].filter((f) => f.get('display') !== false).map((dataSetField) => {
        const { name } = dataSetField;
        const fieldType = dataSetField.get('fieldType');
        const fieldId = dataSetField.get('fieldId');
        const config = getFieldConfig({
          fieldCode: name,
          fieldType,
        });
        const field = find(fields, { fieldCode: name });
        const extraProps = getFieldProps(field);
        return config
          ? (
            <Col
              key={name}
              style={{ marginBottom: 24 }}
              span={lineField.includes(dataSetField.name) ? 24 : 12}
            >
              {config.renderFormItem({
                name,
                fieldId,
                projectId,
                clearButton: !field?.required,
                style: {
                  width: '100%',
                },
                ...extraProps,
              })}
            </Col>
          )
          : null;
      })}
    </Row>
  ));
  return (
    <Spin spinning={isLoading || isFieldsLoading}>
      <Form
        dataSet={dataSet}
      >
        {renderFields()}
        <UploadButton
          fileList={fileList}
          onChange={({ fileList: files }) => {
            if (validateFile(files)) {
              setFileList(files);
            }
          }}
        />
      </Form>
    </Spin>
  );
});

export default CreateIssueBase;
