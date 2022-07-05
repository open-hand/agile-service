import React, {useCallback, useEffect, useMemo, useRef, useState,} from 'react';
import {observer} from 'mobx-react-lite';
import {toJS} from 'mobx';
import {CheckBox, DataSet, Form, Select, Spin, TextField,} from 'choerodon-ui/pro';

import {difference, find, map} from 'lodash';
import {FieldType} from 'choerodon-ui/pro/lib/data-set/enum';
import {epicApi, issueApi} from '@/api';
import {IFieldWidthValue, IModalProps, Issue,} from '@/common/types';
import {difference, find, map} from 'lodash';
import {FieldType} from 'choerodon-ui/pro/lib/data-set/enum';
import {epicApi, issueApi} from '@/api';
import {IFieldWidthValue, IModalProps, Issue, IssueApplyType, ISubIssue,} from '@/common/types';
import useIsInProgram from '@/hooks/useIsInProgram';
import CopyRequired from './copy-required';
import styles from './CopyIssue.less';
import {RequiredFieldDs} from '../required-field/useRequiredFieldDataSet';
import {MAX_LENGTH_EPIC_NAME, MAX_LENGTH_SUMMARY} from "@/constants/MAX_LENGTH";
import {RequiredFieldDs} from '../required-field/useRequiredFieldDataSet';
import {ISSUE_EPIC_TYPE_CODE, WATERFALL_TYPE_CODES} from "@/constants/TYPE_CODE";
import EditIssueStore from "@/components/EditIssue/stores/EditIssueStore";
import openCreateNotification from '@choerodon/master/lib/components/notification';
import {v4 as uuidV4} from 'uuid';

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

const allLinkContentCodes = new Map<string, LinkContent>([
  ['issueLinks', {contentCode: 'issueLinks', contentName: '关联任务'}],
  ['attachments', {contentCode: 'attachments', contentName: '附件'}],
  ['knowledgeRelations', {contentCode: 'knowledgeRelations', contentName: '关联知识'}],
  ['predecessors', {contentCode: 'predecessors', contentName: '依赖关系'}],
  ['relatedBacklogs', {contentCode: 'relatedBacklogs', contentName: '关联需求'}],
  ['relatedTestCases', {contentCode: 'relatedTestCases', contentName: '关联测试用例'}],
  ['relatedBranches', {contentCode: 'relatedBranches', contentName: '关联分支'}],
  ['comments', {contentCode: 'comments', contentName: '评论'}],
])

interface Props {
  store: EditIssueStore;
  issue: Issue;
  projectId?:string;
  issueLink: any[];
  modal: IModalProps;
  applyType: IssueApplyType;
  onOk: (issue: Issue) => void;
  closeSprint: any[];
  copyFields: IFieldWidthValue[];
  setCopyHasEpic: (hasEpic: boolean) => void;
}

/**
 * 复制关联内容
 */
interface LinkContent {
  contentCode: string;
  contentName: string;
}

const CopyIssue: React.FC<Props> = ({
  store, issue, issueLink, applyType, projectId, modal, onOk, closeSprint, copyFields, setCopyHasEpic,
}) => {
  const [finalFields, setFinalFields] = useState<IFieldWidthValue[]>([]);
  const [linkContents, setLinkContents] = useState<LinkContent[]>([]);
  const { isInProgram } = useIsInProgram();
  const [loading, setLoading] = useState<boolean>(false);
  const requiredFieldsVOArrRef = useRef<RequiredFieldDs[] | null>(null);
  const [selfExtraRequiredFields, setSelfExtraRequiredFields] = useState([]);
  const checkEpicName = useCallback(async (value: string) => {
    if (issue.typeCode === ISSUE_EPIC_TYPE_CODE) {
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
  const isWaterfall = WATERFALL_TYPE_CODES.includes(issue.typeCode || '');
  // @ts-ignore
  const { waterfallIssueVO } = store.getIssue;
  const showCopySubIssue = (!isWaterfall && !!issue.subIssueVOList?.length) || (isWaterfall && !!waterfallIssueVO?.childIssueList?.length);
  const subIssues = (isWaterfall ? waterfallIssueVO?.childIssueList as ISubIssue[] : issue.subIssueVOList) || [];

  const handleUpdate = useCallback(async ({
    name, oldValue, value,
  }) => {
    if (name !== 'fields') {
      return;
    }
    const unCopyFieldCodes = difference(finalFields.map((item) => item.fieldCode), value) || [];
    if (unCopyFieldCodes?.length) {
      const res = await issueApi.project(projectId).checkRequiredFields(issue.issueTypeId, unCopyFieldCodes as string[]);
      setSelfExtraRequiredFields(res);
    } else {
      setSelfExtraRequiredFields([]);
    }
    setCopyHasEpic(value.find((code: string) => code === 'epic'));
  }, [finalFields, issue.issueTypeId, projectId, setCopyHasEpic]);

  useEffect(() => {
    setLoading(true);
    issueApi.getNotEmptyLinkContentCodes(issue.issueId)
      .then(res => {
        setLinkContents(res.map(code => allLinkContentCodes.get(code) as LinkContent));
      })
      .finally(() => setLoading(false))
  }, [issue?.issueId])

  const copyIssueDataSet = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [{
      name: 'summary',
      label: '概要',
      type: FieldType.string,
      required: true,
      maxLength: MAX_LENGTH_SUMMARY,
    }, {
      name: 'epicName',
      label: '史诗名称',
      type: FieldType.string,
      validator: checkEpicName,
      required: issue.typeCode === 'issue_epic',
      maxLength: MAX_LENGTH_EPIC_NAME,
    }, {
      name: 'fields',
      label: '复制字段',
      multiple: true,
      textField: 'fieldName',
      valueField: 'fieldCode',
    }, {
      name: 'linkContents',
      label: '复制关联内容',
      multiple: true,
      type: FieldType.string,
      textField: 'contentName',
      valueField: 'contentCode',
    }, {
      name: 'copySubIssue',
      label: isWaterfall ? '是否复制子级工作项' : '是否复制子任务',
      type: 'boolean' as FieldType,
    }],
    data: [{
      summary: issue.summary,
      epicName: issue.typeCode === 'issue_epic' && issue.epicName,
      fields: map(finalFields, 'fieldCode'),
      linkContents: linkContents.map((linkContent) => linkContent.contentCode),
    }],
    events: {
      update: handleUpdate,
    },
  }), [checkEpicName, finalFields, handleUpdate, issue.epicName, issue.summary, issue.typeCode, linkContents, isWaterfall]);

  const openNotificationAfterSubmit = useCallback((asyncTraceId: string) => {
    openCreateNotification({
      notificationKey: `agile-clone-issue-${asyncTraceId}`,
      type: 'ws',
      loadStatus: async () => issueApi.project(projectId).queryAsyncCloneStatus(issue.issueId, asyncTraceId),
      messageKey: `agile-clone-issue-${asyncTraceId}`,
      textObject: {
        failed: {
          title: '复制工作项失败',
          description: <span>复制工作项“{issue.summary}”失败，请重新复制。</span>,
          // icon?: string;
        },
        success: {
          title: '复制工作项成功',
          description: <span>复制工作项成功，您可以刷新{isWaterfall ? '项目计划' : '所有工作项'}页面即可看到新复制的工作项。</span>,
          // icon?: string;
        },
        doing: {
          title: '正在复制工作项',
          description: <span>您正在复制工作项“{issue.summary}”，该过程可能要持续一段时间，您可以进行其他操作，不会影响复制的进程。</span>,
          // icon?: string;
        },
      },
    });
    return true;
  }, [issue, issue.issueId]);
  const handleSubmit = useCallback(async () => {
    const validate = await copyIssueDataSet.validate();
    let epicNameValidate = true;
    if (issue.typeCode === ISSUE_EPIC_TYPE_CODE) {
      // @ts-ignore
      epicNameValidate = await copyIssueDataSet.current?.getField('epicName')?.checkValidity();
    }
    if (!validate || !epicNameValidate) {
      return false;
    }
    return Promise.all((requiredFieldsVOArrRef.current || []).map((item) => item.dataSet.current?.validate())).then(async (validateRes) => {
      if (!validateRes.every((item) => !!item)) {
        return false;
      }
      const copyFieldCodes = copyIssueDataSet.current?.get('fields') || [];
      const copyFields: {
        customFieldIds: string[],
        predefinedFieldNames: string[],
      } = { customFieldIds: [], predefinedFieldNames: [] };
      copyFieldCodes.forEach((copyFieldCode: string) => {
        const field = finalFields.find((f: IFieldWidthValue) => f.fieldCode === copyFieldCode);
        if (field) {
          const { fieldCode, fieldId, system } = field;
          if (!system) {
            copyFields.customFieldIds.push(fieldId);
          } else {
            copyFields.predefinedFieldNames.push(predefinedFieldsMap.get(fieldCode as string) || fieldCode as string);
          }
        }
      });
      if (isInProgram && find(copyFields.predefinedFieldNames, (code) => code === 'epicId')) {
        copyFields.predefinedFieldNames.push('featureId');
      }
      const copyIssueRequiredFieldVOS = (requiredFieldsVOArrRef.current || []).map((item) => item.getData()).map((item) => {
        // 如果包含parentId字段或progress字段, 则单独包装成一个vo
        const finalPredefinedFields = (!!item?.predefinedFields?.parentId || !! item?.predefinedFields?.progress) ?
          {
            ...item?.predefinedFields,
            waterfallIssueVO: {
              parentId: item?.predefinedFields?.parentId,
              progress: item?.predefinedFields?.progress
            },
          } : item?.predefinedFields;
        return {
          customFields: item.customFields,
          predefinedFields: finalPredefinedFields,
          issueId: item.issueIds[0],
        }
      });
      const copyConditionVO = {
        subTask: copyIssueDataSet.current?.get('copySubIssue') || false,
        summary: copyIssueDataSet.current?.get('summary') || false,
        epicName: issue.typeCode === 'issue_epic' && copyIssueDataSet.current?.get('epicName'),
        linkContents: copyIssueDataSet.current?.get('linkContents'),
        copyIssueRequiredFieldVOS,
        ...copyFields,
      };
      const asyncTraceId = uuidV4();
      const res = await issueApi.project(projectId).clone(issue.issueId, applyType, asyncTraceId, copyConditionVO);
      openNotificationAfterSubmit(asyncTraceId);
      onOk(res);
      return true;
    });
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
        <TextField name="summary" showLengthInfo={true} />
        {
          issue.typeCode === 'issue_epic' && (
            <TextField name="epicName" />
          )
        }
        <Select name="fields" style={{ marginBottom: 0 }}>
          {
            finalFields.map((item) => (
              <Option value={item.fieldCode} key={item.fieldCode}>{item.fieldName}</Option>
            ))
          }
        </Select>
        <Select name="linkContents" style={{ marginBottom: !showCopySubIssue ? 10 : 0 }}>
          {
            linkContents.map((item) => (
              <Option value={item.contentCode} key={item.contentCode}>{item.contentName}</Option>
            ))
          }
        </Select>
        {
          (showCopySubIssue) && (
            <div style={{
              position: 'relative',
              top: -6,
              marginBottom: 4,
            }}
            >
              <CheckBox name="copySubIssue" style={{ marginRight: 10 }} />
            </div>
          )
        }
      </Form>
      <CopyRequired
        issue={issue}
        subIssues={subIssues}
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
