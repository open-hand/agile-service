import React, {
  useEffect, useState, useCallback, useRef,
} from 'react';
import { observer } from 'mobx-react-lite';
import { toJS } from 'mobx';
import {
  Icon, Row, Col, Tooltip,
} from 'choerodon-ui';
import { Issue, IField, User } from '@/common/types';
import {
  includes, map, uniq, compact, flatten,
} from 'lodash';
import { unstable_batchedUpdates as batchedUpdates } from 'react-dom';
import DataSetField from 'choerodon-ui/pro/lib/data-set/Field';
import TypeTag from '@/components/TypeTag';
import { DataSet } from 'choerodon-ui/pro/lib';
import {
  fieldApi, moveIssueApi, issueApi, userApi,
} from '@/api';
import styles from './Confirm.less';
import transformValue, { IFieldWithValue } from './transformValue';
import renderField from './renderField';
import store from '../../store';

export interface IssueWithSubIssueVOList extends Omit<Issue, 'subIssueVOList'> {
  subIssueVOList: Issue[]
}

interface Props {
  issue: any,
  dataSet: DataSet,
  fieldsWithValue: IFieldWithValue[]
  targetProjectType: 'program' | 'project' | 'subProject'
  targetIssueType: {
    typeCode: string,
    issueTypeId: string
  }
  dataRef: React.MutableRefObject<Map<string, any>>,
}

const Confirm: React.FC<Props> = ({
  issue, dataSet, fieldsWithValue, targetProjectType, targetIssueType, dataRef,
}) => {
  const {
    selfFields, subTaskFields, moveToProjectList, subTaskDetailMap, subTaskTypeId, selectedUserIds, selectedUsers,
  } = store;
  const [fieldsLosed, setFieldsLosed] = useState<IField[]>([]);
  const {
    issueId, issueTypeVO, issueNum, summary, typeCode, subIssueVOList, epicName,
  } = issue;
  const targetProjectId = dataSet?.current?.get('targetProjectId');
  const issueType = dataSet?.current?.get('issueType');

  const addField = useCallback((name, props) => {
    const field = new DataSetField({ ...props, name }, dataSet, dataSet.current);
    dataSet?.current?.fields.set(name, field);
  }, [dataSet]);

  const filterFields = useCallback((arr: IField[]) => {
    const excludeCodes: string[] = ['summary', 'issueType', 'description', 'remainingTime', 'storyPoints', 'priority', 'estimatedStartTime', 'estimatedEndTime', 'benfitHypothesis', 'acceptanceCritera', 'environment'];
    if (typeCode === 'feature') {
      excludeCodes.push('sprint');
    }
    return arr.filter((item) => (item.system || (!item.system && !item.projectId && item.fieldType === 'member')) && !includes(excludeCodes, item.fieldCode));
  }, [typeCode]);

  const getFinalFields = useCallback((res) => {
    const statusField = {
      fieldName: '状态',
      fieldCode: 'status',
      system: true,
      code: 'status',
      required: true,
    } as IField;

    const reporterField = {
      fieldName: '报告人',
      fieldCode: 'reporter',
      system: true,
      code: 'reporter',
      required: true,
    } as IField;
    const resAdded = [
      statusField,
      ...(res || []),
      reporterField,
    ];
    const filtered = filterFields(resAdded);
    if (targetProjectType === 'subProject') {
      const epicFieldIndex = filtered.findIndex((item) => item.fieldCode === 'epic');
      if (epicFieldIndex > -1) {
        const featureField = {
          fieldName: '特性',
          fieldCode: 'feature',
          system: true,
          code: 'feature',
        } as IField;
        filtered.splice(epicFieldIndex, 1, featureField);
      }
    }
    return filtered;
  }, [filterFields, targetProjectType]);

  useEffect(() => {
    if (targetProjectId && issueType) {
      fieldApi.getFields({
        context: issueType,
        pageCode: 'agile_issue_create',
        schemeCode: 'agile_issue',
      }, targetProjectId).then((res: IField[]) => {
        const finalFields = getFinalFields(res || []);
        finalFields.forEach((item) => {
          if (item.fieldCode !== 'epicName') {
            addField(`${issueId}-${item.fieldCode}`, {
              required: item.required,
            });
          } else {
            addField(`${issueId}-${item.fieldCode}`, {
              required: item.required,
              maxLength: 20,
            });
          }
        });
        store.setSelfFields(finalFields);
      });
      if (subIssueVOList && subIssueVOList.length) {
        fieldApi.getFields({
          context: 'sub_task',
          pageCode: 'agile_issue_create',
          schemeCode: 'agile_issue',
        }, targetProjectId).then((res: IField[]) => {
          const finalFields = getFinalFields(res || []);
          subIssueVOList.forEach((subTask: Issue) => {
            finalFields.forEach((item) => {
              addField(`${subTask.issueId}-${item.fieldCode}`, {
                required: item.required,
              });
            });
          });
          store.setSubTaskFields(finalFields);
        });
      }
    }
  }, [addField, filterFields, getFinalFields, issueId, issueType, subIssueVOList, targetProjectId, targetProjectType]);

  useEffect(() => {
    if (targetProjectId && issueId && targetIssueType.typeCode) {
      moveIssueApi.getFieldsLosed(targetProjectId, issueId, targetIssueType.typeCode).then((res: IField[]) => {
        setFieldsLosed(res);
      });
    }
  }, [issueId, targetIssueType.typeCode, targetProjectId]);

  useEffect(() => {
    if (subTaskTypeId) {
      const detailRequestArr: Promise<Issue>[] = [];
      const customFieldsRequestArr: Promise<IFieldWithValue>[] = [];
      subIssueVOList.forEach((subTask: any) => {
        detailRequestArr.push(issueApi.load(subTask.issueId));
        customFieldsRequestArr.push(fieldApi.getFieldAndValue(subTask.issueId, {
          schemeCode: 'agile_issue',
          context: 'sub_task',
          pageCode: 'agile_issue_edit',
        }));
      });
      let subTaskSelectedUserIds: any[] = [...store.selectedUserIds];
      Promise.all(detailRequestArr).then((res: Issue[]) => {
        res.forEach((subTask) => {
          subTaskSelectedUserIds = [...subTaskSelectedUserIds, ...[subTask.assigneeId, subTask.reporterId, subTask.mainResponsible?.id]];
          subTaskDetailMap.set(`${subTask.issueId}-detail`, subTask);
        });
        const uniqUserIds = uniq(compact(subTaskSelectedUserIds));
        store.setSelectUserIds(uniqUserIds);
      });
      Promise.all(customFieldsRequestArr).then((res: IFieldWithValue[]) => {
        res.forEach((fieldWidthValue, i) => {
          fieldsWithValue.forEach((item) => {
            if (!item.system && item.fieldType === 'member' && !item.projectId) {
              subTaskSelectedUserIds = [...subTaskSelectedUserIds, item.value];
            }
          });
          const uniqUserIds = uniq(compact(subTaskSelectedUserIds));
          store.setSelectUserIds(uniqUserIds);
          subTaskDetailMap.set(`${subIssueVOList[i].issueId}-fields`, fieldWidthValue);
        });
      });
    }
  }, [fieldsWithValue, subIssueVOList, subTaskDetailMap, subTaskTypeId]);

  const targetProject = moveToProjectList.find((item: any) => item.id === targetProjectId) || { name: '' };

  useEffect(() => {
    const selectedUserRequestArr: Promise<any[]>[] = [];
    if (selectedUserIds && selectedUserIds.length) {
      selectedUserIds.forEach((id) => {
        selectedUserRequestArr.push(userApi.project(targetProjectId).getById(id));
      });
      Promise.all(selectedUserRequestArr).then((res) => {
        store.setSelectedUsers(flatten(flatten(res).map((item) => item.list)));
      });
    }
  }, [selectedUserIds, targetProjectId]);

  useEffect(() => {
    const memberFieldsCodeAndValue = new Map([
      [`${issue.issueId}-assignee`, issue.assigneeId],
      [`${issue.issueId}-reporter`, issue.reporterId],
      [`${issue.issueId}-mainResponsible`, issue.mainResponsible?.id],
    ]);
    fieldsWithValue.forEach((field) => {
      const {
        fieldCode, projectId, fieldType, system, value,
      } = field;
      if (!system && !projectId && fieldType === 'member' && value) {
        memberFieldsCodeAndValue.set(`${issue.issueId}-${fieldCode}`, value);
      }
    });

    for (const [k, v] of subTaskDetailMap.entries()) {
      const subTaskIssueId = k.split('-')[0];
      const isDetail = k.split('-')[1] === 'detail';
      if (isDetail) {
        memberFieldsCodeAndValue.set(`${subTaskIssueId}-assignee`, v.assigneeId);
        memberFieldsCodeAndValue.set(`${subTaskIssueId}-reporter`, v.reporterId);
        memberFieldsCodeAndValue.set(`${subTaskIssueId}-mainResponsible`, v.mainResponsible?.id);
      } else {
        v.forEach((field: IFieldWithValue) => {
          const {
            fieldCode, projectId, fieldType, system, value,
          } = field;
          if (!system && !projectId && fieldType === 'member' && value) {
            memberFieldsCodeAndValue.set(`${subTaskIssueId}-${fieldCode}`, value);
          }
        });
      }
    }

    for (const [k, v] of memberFieldsCodeAndValue.entries()) {
      if (v && selectedUsers.find((user) => user.id === v)) {
        dataSet.current?.set(k, v);
      }
    }
  }, [dataSet, fieldsWithValue, issue.assigneeId, issue.issueId, issue.mainResponsible?.id, issue.reporterId, selectedUsers, subTaskDetailMap]);

  useEffect(() => {
    if (epicName && !dataSet.current?.get(`${issueId}-epicName`)) {
      dataSet.current?.set(`${issueId}-epicName`, epicName);
    }
  }, [dataSet, epicName, issueId]);
  return (
    <div className={styles.confirm}>
      <div className={styles.tip}>
        <Icon type="report" />
        <p className={styles.tipText}>
          {`${issue.issueNum}将移动到【${targetProject?.name}】中${fieldsLosed.length > 0 ? `，其${fieldsLosed.map((item) => `【${item.name}】`).join('、')}的字段值，与该问题的问题项、测试用例、文档、需求的关联关系，会永久丢失` : ''}。`}
        </p>
      </div>
      <div className={styles.content}>
        <div className={styles.contentTip}>
          系统将保留兼容的字段值，您可以根据需要更新以下不兼容的字段值：
        </div>
        <div className={styles.contentMain}>
          <div className={styles.issueItem}>
            <div className={styles.issueItemHeader}>
              <TypeTag data={issueTypeVO} />
              <span className={styles.issueNum}>{issueNum}</span>
              <span className={styles.summary}>{summary}</span>
            </div>
            <div className={styles.issueItemFields}>
              <Row key={`${issue.issueId}-fieldHeader`} className={styles.fieldHeaderRow}>
                <Col span={7}>
                  <span className={styles.fieldHeader}>字段</span>
                </Col>
                <Col span={8}>
                  <span className={styles.fieldHeader}>原始值</span>
                </Col>
                <Col span={9}>
                  <span className={styles.fieldHeader}>更新值</span>
                </Col>
              </Row>
              {
                selfFields.map((selfField) => {
                  const { fieldCode, fieldName } = selfField;
                  const transformedOriginValue = transformValue({ issue, field: selfField, fieldsWithValue });
                  return (
                    <Row key={fieldCode} className={styles.fieldRow}>
                      <Col span={7}>
                        <span className={`${styles.fieldReadOnly} ${styles.fieldNameCol}`}>
                          {fieldName}
                          {
                            dataSet.current?.getField(`${issueId}-${fieldCode}`)?.props?.required && (
                              <span className={styles.required}>*</span>
                            )
                          }
                        </span>
                      </Col>
                      <Col span={8}>
                        <Tooltip title={transformedOriginValue}>
                          <span className={styles.fieldReadOnly}>{transformedOriginValue}</span>
                        </Tooltip>
                      </Col>
                      <Col span={9}>
                        {renderField({
                          dataSet,
                          issue,
                          field: selfField,
                          fieldsWithValue,
                          targetIssueType,
                          targetProject: {
                            projectId: targetProjectId,
                            projectType: targetProjectType,
                          },
                          dataRef,
                          selectedUsers,
                          isSelf: true,
                        })}
                      </Col>
                    </Row>
                  );
                })
              }
            </div>
          </div>
          {
            subTaskTypeId ? subIssueVOList.map((subTask: any) => (
              <div className={styles.issueItem}>
                <div className={styles.issueItemHeader}>
                  <TypeTag data={subTask.issueTypeVO} />
                  <span className={styles.issueNum}>{subTask.issueNum}</span>
                  <span className={styles.summary}>{subTask.summary}</span>
                </div>
                <div className={styles.issueItemFields}>
                  <Row key={`${subTask.issueId}-fieldHeader`} className={styles.fieldHeaderRow}>
                    <Col span={7}>
                      <span className={styles.fieldHeader}>字段</span>
                    </Col>
                    <Col span={8}>
                      <span className={styles.fieldHeader}>原始值</span>
                    </Col>
                    <Col span={9}>
                      <span className={styles.fieldHeader}>更新值</span>
                    </Col>
                  </Row>
                  {
                  subTaskFields.map((subTaskField) => {
                    const { fieldCode, fieldName } = subTaskField;
                    const subTaskDetail = subTaskDetailMap.get(`${subTask.issueId}-detail`) || {};
                    const subTaskCustomFields = subTaskDetailMap.get(`${subTask.issueId}-fields`) || [];
                    const transformedOriginValue = transformValue({ issue: subTaskDetail, field: subTaskField, fieldsWithValue: subTaskCustomFields });
                    return (
                      <Row key={fieldCode} className={styles.fieldRow}>
                        <Col span={7}>
                          <span className={`${styles.fieldReadOnly} ${styles.fieldNameCol}`}>
                            {fieldName}
                            {
                              dataSet.current?.getField(`${subTask.issueId}-${fieldCode}`)?.props?.required && (
                                <span className={styles.required}>*</span>
                              )
                            }
                          </span>
                        </Col>
                        <Col span={8}>
                          <Tooltip title={transformedOriginValue}>
                            <span className={styles.fieldReadOnly}>{transformedOriginValue}</span>
                          </Tooltip>
                        </Col>
                        <Col span={9}>
                          {renderField({
                            dataSet,
                            issue: subTaskDetail,
                            field: subTaskField,
                            fieldsWithValue: subTaskCustomFields,
                            targetIssueType: {
                              typeCode: 'sub_task',
                              issueTypeId: subTaskTypeId,
                            },
                            targetProject: {
                              projectId: targetProjectId,
                              projectType: targetProjectType,
                            },
                            dataRef,
                            selectedUsers,
                          })}
                        </Col>
                      </Row>
                    );
                  })
              }
                </div>
              </div>
            )) : null
          }
        </div>
      </div>
    </div>
  );
};

export default observer(Confirm);
