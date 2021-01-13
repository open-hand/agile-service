import React, {
  useMemo, useState, useEffect, useCallback, useRef,
} from 'react';
import { observer } from 'mobx-react-lite';
import {
  DataSet, Modal, Button,
} from 'choerodon-ui/pro';
import { Steps } from 'choerodon-ui';
import {
  includes, map, compact, uniq,
} from 'lodash';
import { toJS } from 'mobx';
import {
  IModalProps, AppStateProps, IIssueType, IField,
} from '@/common/types';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import { ButtonColor, FuncType } from 'choerodon-ui/pro/lib/button/enum';
import { stores, Choerodon } from '@choerodon/boot';
import {
  issueTypeApi, projectApi, moveIssueApi, commonApi,
} from '@/api';
import Field from 'choerodon-ui/pro/lib/data-set/Field';
import SelectProject from './components/select-project';
import Confirm from './components/confirm-data';
import styles from './IssueMove.less';
import { IssueWithSubIssueVOList } from './components/confirm-data/Confirm';
import transformValue, { submitFieldMap } from './transformValue';
import { IFieldWithValue } from './components/confirm-data/transformValue';

import store from './store';

const isDEV = process.env.NODE_ENV === 'development';
// @ts-ignore
const HAS_AGILE_PRO = C7NHasModule('@choerodon/agile-pro');
const shouldRequest = isDEV || HAS_AGILE_PRO;
const { AppState }: { AppState: AppStateProps } = stores;
const { Step } = Steps;

interface Props {
  issue: IssueWithSubIssueVOList,
  modal?:IModalProps
  fieldsWithValue: IFieldWithValue[]
  onMoveIssue: () => void,
}

const IssueMove: React.FC<Props> = ({
  modal, issue, fieldsWithValue, onMoveIssue,
}) => {
  const { dataMap } = store;
  const [updateCount, setUpdateCount] = useState<number>(0);
  const [currentStep, setCurrentStep] = useState<number>(1);
  const [step1NextDisabled, setsStep1NextDisabled] = useState<boolean>(true);
  const [submitBtnDisable, setSubmitBtnDisable] = useState<boolean>(true);
  const [btnLoading, setBtnLoading] = useState<boolean>(false);
  const [targetProjectType, setTargetProjectType] = useState<'program' | 'project' | 'subProject'>('project');
  const issueTypeDataSet = useMemo(() => new DataSet({
    paging: false,
    data: [],
  }), []);

  const removeField = useCallback((ds: DataSet, name: string) => {
    ds?.fields?.delete(name);
    ds?.current?.fields.delete(name);
  }, []);

  const resetData = useCallback((ds: DataSet, excludeFieldsCode: string[]) => {
    const fieldNames: string[] = [];
    (ds.current?.fields || []).forEach((field: Field) => {
      if (!includes(excludeFieldsCode, field.get('name'))) {
        field.reset();
        ds.current?.set(field.get('name'), undefined);
        fieldNames.push(field.get('name'));
      }
    });
    fieldNames.forEach((name: string) => {
      removeField(ds, name);
    });
    store.dataMap.clear();
  }, [removeField]);

  const dataSet = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [
      {
        name: 'targetProjectId',
        textField: 'name',
        valueField: 'id',
        label: '选择目标项目',
        required: true,
      },
      {
        name: 'issueType',
        textField: 'name',
        valueField: 'typeCode',
        label: '选择目标类型',
        options: issueTypeDataSet,
        required: true,
        dynamicProps: {
          disabled: ({ record }) => issue.typeCode === 'feature' || !record.get('targetProjectId'),
        },
      },
    ],
    events: {
      update: async ({
        // @ts-ignore
        dataSet: moveDataSet, name, value,
      }) => {
        if (name === 'targetProjectId') {
          if (value) {
            const targetProjectInfo = await projectApi.loadBasicInfo(value);
            let targetIsInProgram = false;
            const targetIsProgram = targetProjectInfo.category === 'PROGRAM';
            if (!targetIsInProgram && shouldRequest) {
              targetIsInProgram = Boolean(await commonApi.getProjectsInProgram(value));
            }
            const issueTypes: IIssueType[] = await issueTypeApi.loadAllWithStateMachineId(targetIsProgram ? 'program' : 'agile', value);
            const excludeTypeCode: string[] = ['sub_task'];
            if (!targetIsProgram) {
              excludeTypeCode.push('feature');
              if (targetIsInProgram) {
                setTargetProjectType('subProject');
                excludeTypeCode.push('issue_epic');
              } else {
                setTargetProjectType('project');
              }
              if (issue.subIssueVOList && issue.subIssueVOList.length) {
                excludeTypeCode.push('bug');
                store.setSubTaskTypeId((issueTypes || []).find((item: IIssueType) => item.typeCode === 'sub_task')?.id);
              }
            } else {
              setTargetProjectType('program');
            }
            await issueTypeDataSet.loadData(issueTypes.filter((item) => !includes(excludeTypeCode, item.typeCode)));
          } else {
            issueTypeDataSet.loadData([]);
          }
          if (issue.typeCode === 'feature') {
            moveDataSet.current.set('issueType', 'feature');
          } else if (moveDataSet.current?.get('issueType')) {
            moveDataSet.current.set('issueType', undefined);
          }
        }
        if (name === 'targetProjectId' || name === 'issueType') {
          resetData(moveDataSet, ['targetProjectId', 'issueType']); // 改变项目或者问题类型应该重置

          Promise.all([moveDataSet.current?.getField('targetProjectId')?.checkValidity(), moveDataSet.current?.getField('issueType')?.checkValidity()]).then((validateRes) => {
            setsStep1NextDisabled(!validateRes.every((validate) => !!validate));
          });
        }
        if (name === `${issue.issueId}-sprint` && issue.subIssueVOList && issue.subIssueVOList.length) {
          issue.subIssueVOList.forEach((subTask) => {
            moveDataSet.current?.set(`${subTask.issueId}-sprint`, value);
          });
        }
        if (name !== 'targetProjectId' && name !== 'issueType') {
          const validate = await moveDataSet.validate();
          setSubmitBtnDisable(!validate);
        }
        setUpdateCount((count) => count + 1);
      },
    },
  }), [issue.issueId, issue.subIssueVOList, issue.typeCode, issueTypeDataSet, resetData]);

  const handlePre = () => {
    setCurrentStep(currentStep - 1);
  };
  const handleNext = async () => {
    setCurrentStep(currentStep + 1);
  };
  const handleCancel = () => {
    dataSet.reset();
    modal?.close();
  };

  useEffect(() => {
    const userIds = [issue.assigneeId, issue.reporterId, issue.mainResponsible?.id, ...(map(fieldsWithValue.filter((item) => !item.system && item.fieldType === 'member' && !item.projectId), 'value'))];
    const uniqUserIds = uniq(compact(userIds));
    store.setSelectUserIds(uniqUserIds);
  }, [fieldsWithValue, issue.assigneeId, issue.mainResponsible?.id, issue.reporterId]);

  const { selfFields, subTaskFields } = store;
  const targetTypeCode = dataSet.current?.get('issueType');
  const targetProjectId = dataSet.current?.get('targetProjectId');

  const handleSubmit = useCallback(async () => {
    setBtnLoading(true);
    let submitData: any = {
      issueId: issue.issueId,
      typeCode: targetTypeCode,
      subIssues: [],
    };
    for (const [k, v] of Object.entries(dataSet.current?.data || {})) {
      const kIssueId = k.split('-')[0];
      const isSelf = kIssueId === issue.issueId;
      if (kIssueId && kIssueId !== 'undefined' && k.split('-')[1] && v) {
        const fieldInfo = (isSelf ? selfFields : subTaskFields).find((item: IField) => item.fieldCode === k.split('-')[1]);
        if (fieldInfo) {
          if (fieldInfo.system) { // 系统字段
            if (submitFieldMap.get(k.split('-')[1])) {
              const fieldAndValue = {
                [submitFieldMap.get(k.split('-')[1]) as string]: transformValue({
                  k,
                  v,
                  dataMap,
                  targetProjectId,
                }),
              };
              if (isSelf) {
                submitData = {
                  ...submitData,
                  ...fieldAndValue,
                };
                if (k.indexOf('fixVersion') > -1) {
                  submitData = {
                    ...submitData,
                    versionType: 'fix',
                  };
                }
              } else {
                const currentSubIssueItemIndex = submitData.subIssues.findIndex((item: any) => item.issueId === kIssueId);
                if (currentSubIssueItemIndex === -1) {
                  submitData.subIssues.push({
                    issueId: kIssueId,
                    ...fieldAndValue,
                  });
                } else {
                  submitData.subIssues[currentSubIssueItemIndex] = {
                    ...submitData.subIssues[currentSubIssueItemIndex],
                    ...fieldAndValue,
                  };
                }
                if (k.indexOf('fixVersion') > -1) {
                  submitData.subIssues[currentSubIssueItemIndex] = {
                    ...submitData.subIssues[currentSubIssueItemIndex],
                    versionType: 'fix',
                  };
                }
              }
            }
          } else if (isSelf) { // 非系统字段，自己
            if (submitData.customFields) {
              submitData.customFields.push({
                fieldId: fieldInfo.fieldId,
                fieldType: fieldInfo.fieldType,
                value: v,
              });
            } else {
              submitData.customFields = [{
                fieldId: fieldInfo.fieldId,
                fieldType: fieldInfo.fieldType,
                value: v,
              }];
            }
          } else { // 非系统字段，子任务
            const currentSubIssueItemIndex = submitData.subIssues.findIndex((item: any) => item.issueId === kIssueId);
            if (currentSubIssueItemIndex === -1) {
              submitData.subIssues.push({
                issueId: kIssueId,
                customFields: [
                  {
                    fieldId: fieldInfo.fieldId,
                    fieldType: fieldInfo.fieldType,
                    value: v,
                  },
                ],
              });
            } else if (submitData.subIssues[currentSubIssueItemIndex].customFields) {
              submitData.subIssues[currentSubIssueItemIndex].customFields.push({
                fieldId: fieldInfo.fieldId,
                fieldType: fieldInfo.fieldType,
                value: v,
              });
            } else {
              submitData.subIssues[currentSubIssueItemIndex].customFields = [
                {
                  fieldId: fieldInfo.fieldId,
                  fieldType: fieldInfo.fieldType,
                  value: v,
                },
              ];
            }
          }
        }
      }
    }
    moveIssueApi.moveIssueToProject(issue.issueId, targetProjectId, submitData).then(() => {
      dataSet.reset();
      onMoveIssue();
      Choerodon.prompt('移动成功');
      setTimeout(() => {
        setBtnLoading(false);
      }, 2000);
      modal?.close();
    }).catch(() => {
      setBtnLoading(false);
      Choerodon.prompt('移动失败');
    });
    return false;
  }, [dataMap, dataSet, issue.issueId, selfFields, subTaskFields, targetProjectId, targetTypeCode]);

  const targetIssueType = issueTypeDataSet.toData().find((item: IIssueType) => item.typeCode === targetTypeCode) as IIssueType;
  return (
    <div className={styles.issueMove}>
      <Steps current={currentStep - 1}>
        <Step
          title={<span style={{ color: currentStep === 1 ? '#3F51B5' : '', fontSize: 14 }}>选择项目和问题类型</span>}
        />
        <Step
          title={<span style={{ color: currentStep === 2 ? '#3F51B5' : '', fontSize: 14 }}>确认数据信息</span>}
        />
      </Steps>
      <div className={styles.step_content}>
        {currentStep === 1 && <SelectProject issue={issue} dataSet={dataSet} issueTypeDataSet={issueTypeDataSet} />}
        {currentStep === 2 && <Confirm issue={issue} dataSet={dataSet} fieldsWithValue={fieldsWithValue} targetProjectType={targetProjectType} targetIssueType={targetIssueType} />}
      </div>
      <div className={styles.steps_action}>
        {currentStep === 1 && (
          <>
            <Button color={'primary' as ButtonColor} funcType={'raised' as FuncType} onClick={handleNext} disabled={step1NextDisabled}>
              下一步
            </Button>
            <Button onClick={handleCancel} funcType={'raised' as FuncType}>
              取消
            </Button>
          </>
        )}
        {currentStep === 2 && (
          <>
            <Button style={{ marginLeft: 8 }} color={'primary' as ButtonColor} funcType={'raised' as FuncType} onClick={handlePre}>
              上一步
            </Button>
            <Button color={'primary' as ButtonColor} funcType={'raised' as FuncType} onClick={handleSubmit} disabled={submitBtnDisable} loading={btnLoading}>
              确认
            </Button>
            <Button onClick={handleCancel} funcType={'raised' as FuncType}>
              取消
            </Button>
          </>
        )}
      </div>
    </div>
  );
};

const ObserverIssueMove = observer(IssueMove);

const openIssueMove = ({ issue, customFields, onMoveIssue }: { issue: IssueWithSubIssueVOList, customFields: IFieldWithValue[], onMoveIssue: () => void }) => {
  Modal.open({
    key: 'issueMoveModal',
    drawer: true,
    title: '移动问题',
    className: styles.issueMoveModal,
    style: {
      width: MODAL_WIDTH.middle,
    },
    children: <ObserverIssueMove issue={issue} fieldsWithValue={customFields} onMoveIssue={onMoveIssue} />,
    footer: null,
  });
};

export default openIssueMove;
