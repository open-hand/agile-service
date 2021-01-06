import React, {
  useEffect, useState, useCallback, useRef,
} from 'react';
import { observer } from 'mobx-react-lite';
import {
  Icon, Row, Col, Tooltip,
} from 'choerodon-ui';
import { Issue, IField } from '@/common/types';
import { includes } from 'lodash';
import TypeTag from '@/components/TypeTag';
import { DataSet } from 'choerodon-ui/pro/lib';
import { fieldApi, moveIssueApi } from '@/api';
import styles from './Confirm.less';
import transformValue, { IFieldWithValue } from './transformValue';
import renderField from './renderField';
import store from '../../store';

export interface IssueWithSubIssueVOList extends Omit<Issue, 'subIssueVOList'> {
  subIssueVOList: Issue[]
}

interface Props {
  issue: IssueWithSubIssueVOList,
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
  const { selfFields, subTaskFields, moveToProjectList } = store;
  const [fieldsLosed, setFieldsLosed] = useState<IField[]>([]);
  const {
    issueId, issueTypeVO, issueNum, summary, typeCode, subIssueVOList,
  } = issue;
  const targetProjectId = dataSet?.current?.get('targetProjectId');
  const issueType = dataSet?.current?.get('issueType');

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
    } as IField;

    const reporterField = {
      fieldName: '报告人',
      fieldCode: 'reporter',
      system: true,
      code: 'reporter',
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
        store.setSelfFields(finalFields);
      });
      if (subIssueVOList && subIssueVOList.length) {
        fieldApi.getFields({
          context: 'sub_task',
          pageCode: 'agile_issue_create',
          schemeCode: 'agile_issue',
        }, targetProjectId).then((res: IField[]) => {
          const finalFields = getFinalFields(res || []);
          store.setSubTaskFields(finalFields);
        });
      }
    }
  }, [filterFields, getFinalFields, issueType, subIssueVOList, targetProjectId, targetProjectType]);

  useEffect(() => {
    if (targetProjectId && issueId && targetIssueType.typeCode) {
      moveIssueApi.getFieldsLosed(targetProjectId, issueId, targetIssueType.typeCode).then((res: IField[]) => {
        setFieldsLosed(res);
      });
    }
  }, [issueId, targetIssueType.typeCode, targetProjectId]);

  const targetProject = moveToProjectList.find((item: any) => item.id === targetProjectId) || { name: '' };
  return (
    <div className={styles.confirm}>
      <div className={styles.tip}>
        <Icon type="report" />
        <p className={styles.tipText}>
          {`C7NF-03将移动到【${targetProject?.name}】中${fieldsLosed.length > 0 ? `，其${fieldsLosed.map((item) => `【${item.name}】`).join('、')}的字段值，与该问题的问题项、测试用例、文档、需求的关联关系，会永久丢失` : ''}。`}
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
                        <span className={styles.fieldReadOnly}>{fieldName}</span>
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
                        })}
                      </Col>
                    </Row>
                  );
                })
              }
            </div>
          </div>
          {/* {
            subIssueVOList.map((subTask) => (
              <div className={styles.issueItem}>
                <div className={styles.issueItemHeader}>
                  <TypeTag data={subTask.issueTypeVO} />
                  <span className={styles.issueNum}>{subTask.issueNum}</span>
                  <span className={styles.summary}>{subTask.summary}</span>
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
                  subTaskFields.map((subTaskField) => {
                    const { fieldCode, fieldName, system } = subTaskField;
                    const transformedOriginValue = transformValue({ issue, field: subTaskField, fieldsWithValue });
                    return (
                      <Row key={fieldCode} className={styles.fieldRow}>
                        <Col span={7}>
                          <span className={styles.fieldReadOnly}>{fieldName}</span>
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
                            field: subTaskField,
                            fieldsWithValue,
                            targetIssueType,
                            targetProject: {
                              projectId: targetProjectId,
                              projectType: targetProjectType,
                            },
                            dataRef,
                          })}
                        </Col>
                      </Row>
                    );
                  })
              }
                </div>
              </div>
            ))
          } */}
        </div>
      </div>
    </div>
  );
};

export default observer(Confirm);
