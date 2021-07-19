import React, {
  useMemo, useCallback, useEffect, useState, useRef,
} from 'react';
import { observer } from 'mobx-react-lite';
import { toJS } from 'mobx';
import {
  Form, DataSet, Select, TextField, CheckBox,
} from 'choerodon-ui/pro';
import { Spin } from 'choerodon-ui';
import { find, map } from 'lodash';
import { epicApi, issueApi } from '@/api';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
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
]);

const predefinedFieldsMap = new Map([
  ['status', 'statusId'],
  ['reporter', 'reporterId'],
  ['assignee', 'assigneeId'],
  ['epic', 'epicId'],
  ['mainResponsible', 'mainResponsibleId'],
  ['sprint', 'sprintId'],
  ['priority', 'priorityId'],
]);

interface Props {
  issue: Issue
  issueLink: any[]
  modal: IModalProps
  applyType: 'agile' | 'program'
  onOk: (issue: Issue) => void,
  closeSprint: any[]
  copyFields: IFieldWidthValue[]
}
const CopyIssue: React.FC<Props> = ({
  issue, issueLink, applyType, modal, onOk, closeSprint, copyFields,
}) => {
  const [finalFields, setFinalFields] = useState<IFieldWidthValue[]>([]);
  const { isInProgram } = useIsInProgram();
  const [loading, setLoading] = useState<boolean>(false);
  const requiredFieldsVOArrRef = useRef<RequiredFieldDs[] | null>(null);
  const checkEpicName = useCallback(async (value: string) => {
    if (value && value.trim()) {
      return epicApi.checkName(value)
        .then((res: boolean) => {
          if (res) {
            return '史诗名称重复';
          }
          return true;
        });
    }
    return true;
  }, []);

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
  }), [checkEpicName, finalFields, issue.epicName, issue.summary, issue.typeCode]);

  const handleSubmit = useCallback(async () => {
    const validate = await copyIssueDataSet.validate();
    let epicNameValidate = true;
    if (issue.typeCode === 'issue_epic') {
      // @ts-ignore
      epicNameValidate = await copyIssueDataSet.current?.getField('epicName')?.checkValidity();
    }
    if (validate && epicNameValidate) {
      return Promise.all((requiredFieldsVOArrRef.current || []).map((item) => item.dataSet.validate())).then(async (validateRes) => {
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

          const res = await issueApi.clone(issue.issueId, applyType, copyConditionVO);
          onOk(res);
        }
        return true;
      });
    }
    return false;
  }, [applyType, copyIssueDataSet, finalFields, isInProgram, issue.issueId, issue.typeCode, onOk]);

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

  useEffect(() => {
    modal.update({
      okProps: {
        disabled: loading,
      },
    });
  // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [loading]);

  return (
    <Spin spinning={loading}>
      <Form style={styles.copyIssue} dataSet={copyIssueDataSet}>
        <TextField name="summary" />
        {
        issue.typeCode === 'issue_epic' && (
          <TextField name="epicName" />
        )
      }
        <Select name="fields">
          {
          finalFields.map((item) => (
            <Option value={item.fieldCode} key={item.fieldCode}>{item.fieldName}</Option>
          ))
        }
        </Select>
        <div>
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
      </Form>
      <CopyRequired issue={issue} copySubIssueChecked={copyIssueDataSet.current?.get('copySubIssue') || false} requiredFieldsVOArrRef={requiredFieldsVOArrRef} setLoading={setLoading} />
    </Spin>
  );
};

export default observer(CopyIssue);
