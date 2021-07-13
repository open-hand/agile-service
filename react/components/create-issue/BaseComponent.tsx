import React, {
  useEffect, useRef, useState,
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
import getFieldConfig from './fields';

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
  }) => void,
  modal?: IModalProps,
  projectId?: string,
}
const defaultDataSet = new DataSet({
  autoCreate: true,
  fields: [],
});
const presets = new Map([
  ['estimatedStartTime', {
    max: 'estimatedEndTime',
  }],
  ['estimatedEndTime', {
    min: 'estimatedStartTime',
  }],
]);
const lineField = ['summary', 'description'];
const reuseFields = ['issueType', 'summary', 'description'];

function transformSubmitFieldValue(field: IssueCreateFields, value: any) {
  switch (field.fieldType) {
    case 'time':
    case 'date':
    case 'datetime': {
      return value ? value.format('YYYY-MM-DD HH:mm:ss') : value;
    }
    default: return value;
  }
}
const CreateIssueBase = observer(({
  modal, projectId, onSubmit,
}: CreateIssueBaseProps) => {
  const [fileList, setFileList] = useState<UploadFile[]>();
  const dataSetRef = useRef(defaultDataSet);
  const [dataSet, setDataSet] = useState(defaultDataSet);
  dataSetRef.current = dataSet;
  const setFieldValue = usePersistFn((name, value) => {
    dataSet.current?.set(name, value);
  });
  const { isFetching: isLoading, data: issueTypeList } = useProjectIssueTypes({

  }, {
    onSuccess: ((issueTypes) => {
      setFieldValue('issueType', issueTypes[0].id);
    }),
  });
  const issueTypeId = dataSet.current && dataSet.current.get('issueType');
  const handleUpdate = usePersistFn(({ name, value }) => {
    switch (name) {
      case 'issueType': {
        break;
      }
      default: break;
    }
  });
  const { data: fields, isFetching: isFieldsLoading } = useIssueCreateFields({ issueTypeId });
  useEffect(() => {
    const oldDataSet = dataSetRef.current;
    const newDataSet = new DataSet({
      autoCreate: true,
      fields: fields ? fields.map((field) => {
        const preset = presets.get(field.fieldCode) ?? {};
        return merge(preset, {
          name: field.fieldCode,
          label: field.fieldName,
          required: field.required,
        });
      }) : [],
    });
    // 保留之前的值
    reuseFields.forEach((name) => {
      const oldValue = oldDataSet.current?.get(name);
      if (oldValue) {
        newDataSet?.current?.set(name, oldValue);
      }
    });
    setDataSet(newDataSet);
  }, [fields]);
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
      });
      await onSubmit({
        data: values, fieldList,
      });
      return true;
    }
    return false;
  });
  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);
  const renderFields = usePersistFn(() => (
    <Row gutter={24}>
      {fields?.map((field) => {
        const config = getFieldConfig(field);
        return config ? (
          <Col
            key={field.fieldCode}
            style={{ marginBottom: 24 }}
            span={lineField.includes(field.fieldCode) ? 24 : 12}
          >
            {config.renderFormItem({
              name: field.fieldCode,
              fieldId: field.fieldId,
              projectId,
              style: {
                width: '100%',
              },
            })}
          </Col>
        ) : null;
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
