import React, {
  useMemo, useCallback, useEffect, useState, useRef,
} from 'react';
import { observer } from 'mobx-react-lite';
import { toJS } from 'mobx';
import {
  Form, DataSet, Select, TextField, CheckBox,
  Spin,
} from 'choerodon-ui/pro';

import { difference, find, map } from 'lodash';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { epicApi, issueApi } from '@/api';
import {
  IFieldWidthValue, IModalProps, Issue,
} from '@/common/types';
import useIsInProgram from '@/hooks/useIsInProgram';
import CopyRequired from './copy-required';
import styles from './CopyIssue.less';
import { RequiredFieldDs } from '../required-field/useRequiredFieldDataSet';

const { Option } = Select;
const systemFieldsMap = new Map([
  ['status', 'statusId'],
  ['reporter', 'reporterId'],
  ['assignee', 'assigneeId'],
  ['sprint', 'activeSprint'],
  ['fixVersion', 'versionIssueRelVOList'],
  ['influenceVersion', 'versionIssueRelVOList'],
  ['epic', 'epicId'],
  ['priority', 'priorityId'],
  ['label', 'labelIssueRelVOList'],
  ['component', 'componentIssueRelVOList'],
  ['tag', 'tags'],
  ['participant', 'participants'],
]);

const predefinedFieldsMap = new Map([
  ['status', 'statusId'],
  ['reporter', 'reporterId'],
  ['assignee', 'assigneeId'],
  ['epic', 'epicId'],
  ['mainResponsible', 'mainResponsibleId'],
  ['sprint', 'sprintId'],
  ['priority', 'priorityId'],
  ['participant', 'participantIds'],
]);

interface Props {
  issue: Issue
  projectId?:string
  issueLink: any[]
  modal: IModalProps
  applyType: 'agile' | 'program'
  onOk: (issue: Issue) => void,
  closeSprint: any[]
  copyFields: IFieldWidthValue[]
  setCopyHasEpic: (hasEpic: boolean) => void
}
const CopyIssue: React.FC<Props> = ({
  issue, issueLink, applyType, projectId, modal, onOk, closeSprint, copyFields, setCopyHasEpic,
}) => {
  const [finalFields, setFinalFields] = useState<IFieldWidthValue[]>([]);
  const { isInProgram } = useIsInProgram();
  const [loading, setLoading] = useState<boolean>(false);
  const requiredFieldsVOArrRef = useRef<RequiredFieldDs[] | null>(null);
  const [selfExtraRequiredFields, setSelfExtraRequiredFields] = useState([]);
  const checkEpicName = useCallback(async (value: string) => {
    if (issue.typeCode === 'issue_epic') {
      if (value && value.trim()) {
        return epicApi.project(projectId).checkName(value)
          .then((res: boolean) => {
            if (res) {
              return '史诗名称重复';
            }
            return true;
          });
      }
    }
    return true;
  }, [issue.typeCode, projectId]);

  const handleUpdate = useCallback(async ({
    name, oldValue, value,
  }) => {
    if (name === 'fields') {
      const unCopyFieldCodes = difference(finalFields.map((item) => item.fieldCode), value) || [];
      if (unCopyFieldCodes?.length) {
        const res = await issueApi.project(projectId).checkRequiredFields(issue.issueTypeId, unCopyFieldCodes as string[]);
        setSelfExtraRequiredFields(res);
      } else {
        setSelfExtraRequiredFields([]);
      }
      setCopyHasEpic(value.find((code: string) => code === 'epic'));
    }
  }, [finalFields, issue.issueTypeId, projectId, setCopyHasEpic]);
  const copyIssueDataSet = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [{
      name: 'summary',
      label: '概要',
      type: 'string' as FieldType,
      required: true,
      maxLength: 44,
    }, {
      name: 'epicName',
      label: '史诗名称',
      type: 'string' as FieldType,
      validator: checkEpicName,
      required: issue.typeCode === 'issue_epic',
      maxLength: 20,
    }, {
      name: 'fields',
      label: '复制字段',
      multiple: true,
      textField: 'fieldName',
      valueField: 'fieldCode',
    }, {
      name: 'copySubIssue',
      label: '是否复制子任务',
      type: 'boolean' as FieldType,
    }, {
      name: 'copyLinkIssue',
      label: '是否复制关联任务',
      type: 'boolean' as FieldType,
    }],
    data: [{
      summary: issue.summary,
      epicName: issue.typeCode === 'issue_epic' && issue.epicName,
      fields: map(finalFields, 'fieldCode'),
    }],
    events: {
      update: handleUpdate,
    },
  }), [checkEpicName, finalFields, handleUpdate, issue.epicName, issue.summary, issue.typeCode]);

  const handleSubmit = useCallback(async () => {
    const validate = await copyIssueDataSet.validate();
    let epicNameValidate = true;
    if (issue.typeCode === 'issue_epic') {
      // @ts-ignore
      epicNameValidate = await copyIssueDataSet.current?.getField('epicName')?.checkValidity();
    }
    if (validate && epicNameValidate) {
      return Promise.all((requiredFieldsVOArrRef.current || []).map((item) => item.dataSet.current?.validate())).then(async (validateRes) => {
        if (validateRes.every((item) => !!item)) {
          const fields = copyIssueDataSet.current?.get('fields') || [];
          const copyfs: {
            customFieldIds: string[],
            predefinedFieldNames: string[],
          } = { customFieldIds: [], predefinedFieldNames: [] };
          fields.forEach((item: string) => {
            const field = finalFields.find((f: IFieldWidthValue) => f.fieldCode === item);
            if (field) {
              const { fieldCode, fieldId, system } = field;
              if (!system) {
                copyfs.customFieldIds.push(fieldId);
              } else {
                copyfs.predefinedFieldNames.push(predefinedFieldsMap.get(fieldCode as string) || fieldCode as string);
              }
            }
          });
          if (isInProgram && find(copyfs.predefinedFieldNames, (code) => code === 'epicId')) {
            copyfs.predefinedFieldNames.push('featureId');
          }
          const copyIssueRequiredFieldVOS = (requiredFieldsVOArrRef.current || []).map((item) => item.getData()).map((item) => ({
            customFields: item.customFields,
            predefinedFields: item.predefinedFields,
            issueId: item.issueIds[0],
          }));
          const copyConditionVO = {
            issueLink: copyIssueDataSet.current?.get('copyLinkIssue') || false,
            subTask: copyIssueDataSet.current?.get('copySubIssue') || false,
            summary: copyIssueDataSet.current?.get('summary') || false,
            epicName: issue.typeCode === 'issue_epic' && copyIssueDataSet.current?.get('epicName'),
            copyIssueRequiredFieldVOS,
            ...copyfs,
          };

          const res = await issueApi.project(projectId).clone(issue.issueId, applyType, copyConditionVO);
          onOk(res);
          return true;
        }
        return false;
      });
    }
    return false;
  }, [applyType, copyIssueDataSet, finalFields, isInProgram, issue.issueId, issue.typeCode, onOk, projectId]);
  useEffect(() => {
    modal.handleOk(handleSubmit);
  }, [handleSubmit, modal]);

  useEffect(() => {
    const { typeCode } = issue;
    let fields = [...copyFields];
    // 系统字段单独控制是否显示
    if (typeCode === 'sub_task' || typeCode === 'issue_epic') {
      fields = copyFields.filter((field) => ['epic'].indexOf(field.fieldCode as string) === -1);
    }

    const arr = [];

    for (let i = 0; i < fields.length; i += 1) {
      const { system, fieldCode, value } = fields[i];
      if (system) {
        if (fieldCode === 'sprint' && (issue.activeSprint?.sprintId || issue.closeSprint?.length)) {
          arr.push(fields[i]);
        }
        if (fieldCode === 'epic' && (issue.featureId || issue.epicId)) {
          if (isInProgram) {
            arr.push({ ...fields[i], fieldName: '特性' });
          } else {
            arr.push(fields[i]);
          }
        }
        if (fieldCode === 'influenceVersion' && (issue.versionIssueRelVOList?.length && find(issue.versionIssueRelVOList || [], { relationType: 'influence' }))) {
          arr.push(fields[i]);
        }
        if (fieldCode === 'fixVersion' && (issue.versionIssueRelVOList?.length && find(issue.versionIssueRelVOList || [], { relationType: 'fix' }))) {
          arr.push(fields[i]);
        }
        // @ts-ignore
        const systemValue = toJS(issue[systemFieldsMap.get(fieldCode) || fieldCode]);
        if (Array.isArray(systemValue) ? systemValue?.length > 0 : !!systemValue) {
          if (!find(arr, { fieldCode: fields[i].fieldCode })) {
            arr.push(fields[i]);
          }
        }
      } else if (value) {
        arr.push(fields[i]);
      }
    }

    setFinalFields(arr);
  }, [copyFields, isInProgram, issue]);

  return (
    <Spin spinning={loading}>

      <Form style={styles.copyIssue} dataSet={copyIssueDataSet}>
        <TextField name="summary" />
        {
        issue.typeCode === 'issue_epic' && (
          <TextField name="epicName" />
        )
      }
        <Select name="fields" style={{ marginBottom: !(!!issue.subIssueVOList.length || !!issueLink.length) ? 10 : 0 }}>
          {
            finalFields.map((item) => (
              <Option value={item.fieldCode} key={item.fieldCode}>{item.fieldName}</Option>
            ))
          }
        </Select>
        {
          (!!issue.subIssueVOList.length || !!issueLink.length) && (
          <div style={{
            position: 'relative',
            top: -6,
            marginBottom: 4,
          }}
          >
            {
              !!issue.subIssueVOList.length && (
                <CheckBox name="copySubIssue" style={{ marginRight: 10 }} />
              )
            }
            {
              !!issueLink.length && (
                <CheckBox name="copyLinkIssue" />
              )
            }
          </div>
          )
        }
      </Form>
      <CopyRequired
        issue={issue}
        projectId={projectId}
        copySubIssueChecked={copyIssueDataSet.current?.get('copySubIssue') || false}
        selfExtraRequiredFields={selfExtraRequiredFields}
        requiredFieldsVOArrRef={requiredFieldsVOArrRef}
        setLoading={setLoading}
      />
    </Spin>
  );
};

export default observer(CopyIssue);
