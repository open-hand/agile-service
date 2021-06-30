import React, {
  useEffect, useCallback, useMemo, useState, useRef,
} from 'react';
import {
  Modal, Form, DataSet, Select,
} from 'choerodon-ui/pro';
import {
  includes, find, pick, assign,
} from 'lodash';
import { observer } from 'mobx-react-lite';
import { usePersistFn } from 'ahooks';
import useIsInProgram from '@/hooks/useIsInProgram';
import Field from 'choerodon-ui/pro/lib/data-set/Field';
import { IField, IModalProps } from '@/common/types';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import Loading from '@/components/Loading';
import { issueApi } from '@/api';
import { getProjectId } from '@/utils/common';
import useProjectIssueTypes from '@/hooks/data/useProjectIssueTypes';
import renderField from './renderField';
import styles from './index.less';

const { Option } = Select;
export interface ChangeTypeModalProps {
  modal?: IModalProps,
  requiredFields: IField[]
  issueVO: {
    issueTypeVO: {
      id: string
    }
    summary: string,
    issueId: string
    objectVersionNumber: number
    typeCode: string,
    issueTypeId: string,
  }
  reloadIssue: Function,
  onUpdate: Function,
}

const systemFields = new Map([
  ['description', {
    id: 'description',
  }],
  ['status', {
    id: 'statusId',
  }],
  ['assignee', {
    id: 'assigneeId',
  }],
  ['reporter', {
    id: 'reporterId',
  }],
  ['sprint', {
    id: 'sprintId',
  }],
  ['epic', {
    id: 'epicId',
  }],
  ['featureId', {
    id: 'featureId',
  }],
  ['priority', {
    id: 'priorityId',
  }],
  ['label', {
    id: 'labelIssueRelVOList',
    // @ts-ignore
    format: (value, labelIssueRelVOList) => labelIssueRelVOList,
  }],
  ['component', {
    id: 'componentIssueRelVOList',
    // @ts-ignore
    format: (value, component) => component,
  }],
  ['influenceVersion', {
    id: 'influenceVersion',
    // @ts-ignore
    format: (value, influenceVersion) => pick(influenceVersion, ['versionId', 'name']),
  }],
  ['fixVersion', {
    id: 'fixVersion',
    // @ts-ignore
    format: (value, fixVersion) => pick(fixVersion, ['versionId', 'name']),
  }],
  ['storyPoints', {
    id: 'storyPoints',
  }],
  ['remainingTime', {
    id: 'remainingTime',
  }],
  ['estimatedStartTime', {
    id: 'estimatedStartTime',
  }],
  ['estimatedEndTime', {
    id: 'estimatedEndTime',
  }],
  ['mainResponsible', {
    id: 'mainResponsibleId',
  }],
  ['environment', {
    id: 'environment',
  }],
]);

function transformValue(dataSet: DataSet, key: string, value: any, format: (v: any, lookup: any) => any) {
  if (!value || !format) {
    return value;
  }
  function transform(v: any) {
    const lookup = dataSet.getField(key)?.getLookupData(v);
    return format(v, lookup);
  }
  if (Array.isArray(value)) {
    return value.map((v) => transform(v));
  }
  return transform(value);
}

function formatFields(fieldData: IField[], data: object, dataSet: DataSet, isInProgram: boolean) {
  const temp: {
    predefinedFields: object
    customFields: {
      fieldId: string,
      fieldType: string,
      value: any,
    }[]
  } = {
    predefinedFields: {},
    customFields: [],
  };
  for (const key of Object.keys(data)) {
    const field = fieldData.find((item: IField) => item.fieldCode === key);
    if (systemFields.get(key) || field?.system) {
      // @ts-ignore
      temp.predefinedFields[systemFields.get(key)?.id || field?.fieldCode] = transformValue(dataSet, key, data[key], systemFields.get(key)?.format);
    } else {
      const customField = find(fieldData, { fieldCode: key });
      if (customField) {
        temp.customFields.push({
          fieldId: customField.fieldId,
          fieldType: customField.fieldType,
          // @ts-ignore
          value: data[key],
        });
      }
    }
  }
  return temp;
}

const extraFields = ['timeTrace'];

const ChangeTypeModal: React.FC<ChangeTypeModalProps> = (props) => {
  const { isInProgram } = useIsInProgram();
  let { data: issueTypeData = [] } = useProjectIssueTypes({ onlyEnabled: true });

  const {
    modal, requiredFields: fields, issueVO, reloadIssue, onUpdate,
  } = props;

  const [requiredFields, setRequiredFields] = useState(fields.filter((item) => !includes(extraFields, item.fieldCode)) || []);
  const [loading, setLoading] = useState<boolean>(false);

  const lookupFields = useMemo(() => [{
    name: 'statusId',
    label: '状态',
    lookupAxiosConfig: () => ({
      url: `/agile/v1/projects/${getProjectId()}/schemes/query_status_by_issue_type_id?apply_type=agile&issue_type_id=${issueVO.issueTypeId}`,
      method: 'get',
    }),
    valueField: 'id',
    textField: 'name',
  }, {
    name: 'sprintId',
    label: '冲刺',
    lookupAxiosConfig: () => ({
      url: `/agile/v1/projects/${getProjectId()}/sprint/names`,
      method: 'post',
      data: ['started', 'sprint_planning'],
    }),
    valueField: 'sprintId',
    textField: 'sprintName',
  },
  ...isInProgram ? [{
    name: 'featureId',
    label: '所属特性',
    valueField: 'issueId',
    textField: 'summary',
  }] : [{
    name: 'epicId',
    label: '所属史诗',
    lookupAxiosConfig: () => ({
      url: `/agile/v1/projects/${getProjectId()}/issues/epics/select_data`,
      method: 'get',
    }),
    valueField: 'issueId',
    textField: 'epicName',
  }], {
    name: 'priorityId',
    label: '优先级',
    lookupAxiosConfig: () => ({
      url: `/agile/v1/projects/${getProjectId()}/priority/list_by_org`,
      method: 'get',
      transformResponse: (response: any) => {
        try {
          const data = JSON.parse(response);
          return data.filter((v: any) => v.enable);
        } catch (error) {
          return response;
        }
      },
    }),
    valueField: 'id',
    textField: 'name',
  }, {
    name: 'labelIssueRelVOList',
    label: '标签',
    lookupAxiosConfig: () => ({
      url: `/agile/v1/projects/${getProjectId()}/issue_labels`,
      method: 'get',
    }),
    valueField: 'labelId',
    textField: 'labelName',
  }, {
    name: 'componentIssueRelVOList',
    label: '模块',
    lookupAxiosConfig: ({ params }: { params: any}) => ({
      url: `/agile/v1/projects/${getProjectId()}/component/query_all`,
      method: 'post',
      data: {
        advancedSearchArgs: {},
        searchArgs: { name: params.name },
      },
      params: {
        size: 999,
        page: 1,
      },
      transformResponse: (response:any) => {
        try {
          const data = JSON.parse(response);
          return data.content;
        } catch (error) {
          return response;
        }
      },
    }),
    valueField: 'componentId',
    textField: 'name',
  }, {
    name: 'fixVersion',
    label: '修复的版本',
    lookupAxiosConfig: () => ({
      url: `/agile/v1/projects/${getProjectId()}/product_version/names`,
      method: 'post',
      data: ['version_planning'],
    }),
    valueField: 'versionId',
    textField: 'name',
  }, {
    name: 'influenceVersion',
    label: '影响的版本',
    lookupAxiosConfig: () => ({
      url: `/agile/v1/projects/${getProjectId()}/product_version/names`,
      method: 'post',
      data: [],
    }),
    valueField: 'versionId',
    textField: 'name',
  }], [isInProgram, issueVO.issueTypeId]);

  const changeTypeDataSet = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [
      ...(requiredFields.map((item) => {
        const key = item.fieldCode === 'epic' && isInProgram ? 'featureId' : (systemFields.get(item.fieldCode as string)?.id || item?.fieldCode);
        const lookupField = {};
        if (item.system && lookupFields.find((field) => field.name === key)) {
          assign(lookupField, lookupFields.find((field) => field.name === key));
        }
        return ({
          ...lookupField,
          name: item.fieldCode === 'epic' && isInProgram ? 'featureId' : item.fieldCode,
          label: item.fieldCode === 'epic' && isInProgram ? '特性' : item.fieldName,
          required: true,
        });
      }))],

  }), [isInProgram, lookupFields, requiredFields]);

  const removeField = useCallback((name: string) => {
    changeTypeDataSet?.fields?.delete(name);
    changeTypeDataSet?.current?.fields.delete(name);
  }, [changeTypeDataSet]);

  const resetDataRef = useRef<(excludeFieldsCode: string[]) => void>(() => {});
  const resetData = useCallback((excludeFieldsCode: string[]) => {
    const fieldNames: string[] = [];
    (changeTypeDataSet.current?.fields || []).forEach((field: Field) => {
      if (!includes(excludeFieldsCode, field.get('name'))) {
        field.reset();
        changeTypeDataSet.current?.set(field.get('name'), undefined);
        fieldNames.push(field.get('name'));
      }
    });
    fieldNames.forEach((name: string) => {
      removeField(name);
    });
  }, [changeTypeDataSet, removeField]);

  resetDataRef.current = resetData;

  const issueTypeIdDataSet = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [{
      name: 'issueTypeId',
      label: '问题类型',
      required: true,
      defaultValue: issueVO.issueTypeId,
    }],
    events: {
      update: async ({
        // @ts-ignore
        dataSet, name, value,
      }) => {
        if (name === 'issueTypeId') {
          if (resetDataRef.current) {
            resetDataRef.current(['issueTypeId']);
          }
          if (value) {
            setLoading(true);
            const res = await issueApi.getRequiredField(issueVO.issueId, value);
            setRequiredFields(res.filter((item: IField) => !includes(extraFields, item.fieldCode)));
            setLoading(false);
          } else {
            setRequiredFields([]);
          }
        }
      },
    },
  }), [issueVO.issueId, issueVO.issueTypeId]);

  const getData = () => {
    const temp = changeTypeDataSet.current ? changeTypeDataSet.current.toData() : {};
    const obj = {};
    requiredFields.forEach((field) => {
      if (field.fieldCode) {
        const key = field.fieldCode === 'epic' && isInProgram ? 'featureId' : field.fieldCode;
        assign(obj, { [key]: temp[key] });
      }
    });
    return obj;
  };

  const handleSubmit = usePersistFn(async () => {
    const issueTypeIdValidate = await issueTypeIdDataSet.validate();
    const validate = await changeTypeDataSet.validate();
    if (issueTypeIdValidate && validate) {
      // @ts-ignore
      const data = getData();
      const submitData = requiredFields.length ? {
        issueId: issueVO.issueId,
        objectVersionNumber: issueVO.objectVersionNumber,
        typeCode: find(issueTypeData, { id: issueTypeIdDataSet.current?.get('issueTypeId') })?.typeCode as string,
        issueTypeId: issueTypeIdDataSet.current?.get('issueTypeId') as string,
        batchUpdateFieldsValueVo: {
          issueIds: [issueVO.issueId],
          ...formatFields(requiredFields, data, changeTypeDataSet, isInProgram),
        },
      } : {
        epicName: issueVO.typeCode === 'issue_epic' ? issueVO.summary : undefined,
        issueId: issueVO.issueId,
        objectVersionNumber: issueVO.objectVersionNumber,
        typeCode: find(issueTypeData, { id: issueTypeIdDataSet.current?.get('issueTypeId') })?.typeCode as string,
        issueTypeId: issueTypeIdDataSet.current?.get('issueTypeId') as string,
      };
      const res = await issueApi.updateType(submitData);
      if (reloadIssue) {
        reloadIssue(issueVO.issueId);
      }
      if (onUpdate) {
        onUpdate(res);
      }
      return true;
    }
    return false;
  });

  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);

  useEffect(() => {
    requiredFields.filter((item) => item.defaultValue).forEach(({ fieldCode, defaultValue }) => {
      changeTypeDataSet.current?.set(fieldCode as string, defaultValue);
    });
    if (requiredFields.filter((item) => item.fieldCode === 'epicName')) {
      changeTypeDataSet.current?.set('epicName', issueVO.summary);
    }
  }, [changeTypeDataSet, issueVO.summary, requiredFields]);

  const { stateMachineId } = find(issueTypeData, { id: issueVO.issueTypeVO?.id }) || {};
  issueTypeData = issueTypeData.filter((item) => item.stateMachineId !== stateMachineId).filter((item) => !['feature', ...(issueVO.typeCode === 'sub_task' ? [] : ['sub_task'])].includes(item.typeCode));
  if (isInProgram) {
    issueTypeData = issueTypeData.filter((item) => item.typeCode !== 'issue_epic');
  }

  return (
    <>
      <Loading loading={loading} />
      <Form className={styles.changeType} dataSet={issueTypeIdDataSet} style={{ marginLeft: -5 }}>
        <div className={styles.part_title}>
          类型修改为
        </div>
        <Select name="issueTypeId">
          {
          issueTypeData.map((type) => (
            <Option value={type.id} key={type.id}>{type.name}</Option>
          ))
        }
        </Select>
      </Form>
      {
        !loading && requiredFields.length > 0 && (
          <div className={styles.part_title} style={{ marginTop: 10, marginBottom: 13 }}>补全必输字段</div>
        )
      }
      <Form dataSet={changeTypeDataSet} columns={2} style={{ marginLeft: -5 }}>
        {
        (loading ? [] : requiredFields).map((item) => (renderField({
          field: item, otherComponentProps: {}, dataSet: changeTypeDataSet, isInProgram,
        })))
      }
      </Form>
    </>
  );
};

const ObserverChangeTypeModal = observer(ChangeTypeModal);

const openRequiredFieldsModal = (props: ChangeTypeModalProps) => {
  Modal.open({
    key: 'changeTypeModal',
    className: styles.changeTypeModal,
    title: '修改问题类型',
    drawer: true,
    style: {
      width: MODAL_WIDTH.middle,
    },
    children: <ObserverChangeTypeModal {...props} />,
  });
};
export default openRequiredFieldsModal;
