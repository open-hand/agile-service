import React, {memo, ReactElement, useEffect, useImperativeHandle, useMemo, useState,} from 'react';
import {observer} from 'mobx-react-lite';
import classnames from 'classnames';
import {DataSet} from 'choerodon-ui/pro';
import {unstable_batchedUpdates as batchedUpdates} from 'react-dom';
import {IFieldWidthValue, Issue, ISubIssue} from '@/common/types';
import {defaultIssueType, issueApi} from '@/api';
import RequiredField from '@/components/required-field';
import useRequiredFieldDataSet, {RequiredFieldDs} from '@/components/required-field/useRequiredFieldDataSet';
import styles from './CopyRequired.less';
import SubTaskRequired, {ISubTaskRequiredItem} from './SubTaskRequired';

interface FormPartProps {
  title: string | ReactElement,
  className?: string,
  children: ReactElement | ReactElement[] | null | Array<ReactElement | null>,
  btnOnClick?: (nextBtnStatusCode: 'ALL' | 'NONE') => boolean,
  partStyle?: React.CSSProperties
}

const FormPart: React.FC<FormPartProps> = memo((props) => {
  const {
    title, children, className, partStyle, ...otherProps
  } = props;

  return (
    <div className={classnames(styles.form, className)} style={{ ...(partStyle || {}) }} {...otherProps}>
      <div className={styles.form_title}>
        <div className={styles.form_block} />
        <div>{title}</div>
      </div>
      <div className={styles.form_content}>
        {children}
      </div>
    </div>
  );
});

interface Props {
  issue: Issue;
  subIssues: ISubIssue[];
  copySubIssueChecked: boolean;
  requiredFieldsVOArrRef: React.MutableRefObject<RequiredFieldDs[] | null>;
  setLoading: (loading: boolean) => void;
  selfExtraRequiredFields: IFieldWidthValue[];
  projectId?:string;
}

const CopyRequired: React.FC<Props> = ({
  issue, subIssues, copySubIssueChecked, requiredFieldsVOArrRef, setLoading, selfExtraRequiredFields, projectId,
}) => {
  const [selfTaskRequiredItem, setSelfTaskRequiredItem] = useState<ISubTaskRequiredItem>();
  const [subTaskRequiredItems, setSubTaskRequiredItems] = useState<ISubTaskRequiredItem[]>([]);
  const [hasGotSubTaskRequiredItems, setHasGotSubTaskRequiredItems] = useState<boolean>(false);

  useEffect(() => {
    setLoading(true);
    issueApi.project(projectId).batchGetRequiredField(issue.issueId).then((res) => {
      batchedUpdates(() => {
        if(res && res.length > 0){
          setSelfTaskRequiredItem(res[0]);
        }
      });
    }).finally(() => {
      setLoading(false);
    });
  }, [issue.issueId, issue.issueTypeId, projectId, setLoading]);

  useEffect(() => {
    if (copySubIssueChecked && !hasGotSubTaskRequiredItems) {
      setLoading(true);
      issueApi.project(projectId).batchGetRequiredField(issue.issueId, true).then((res) => {
        batchedUpdates(() => {
          setHasGotSubTaskRequiredItems(true);
          const { issueId } = issue;
          setSubTaskRequiredItems(res?.filter(item => item.issueId !== issueId) || []);
        });
      }).finally(() => {
        setLoading(false);
      });
    }
  }, [copySubIssueChecked, projectId, setLoading]);

  const issueIdToSubTaskRequiredItemMap = useMemo<Map<number|string, ISubTaskRequiredItem>>(() => {
    if (!copySubIssueChecked) {
      return new Map<number|string, ISubTaskRequiredItem>();
    }
    const idToSubIssueMap = subIssues.reduce((pre, cur) => { pre.set(cur.issueId, cur); return pre}, new Map<string, Issue>());
    return subTaskRequiredItems.reduce((map, item) => {
      const subTask = idToSubIssueMap.get(item.issueId);
      item.issueTypeId = subTask?.issueTypeId;
      item.issueTypeVO = subTask?.issueTypeVO || defaultIssueType;
      map.set(item.issueId, item);
      return map;
    }, new Map<number|string, ISubTaskRequiredItem>());
  }, [copySubIssueChecked, subIssues, subTaskRequiredItems]);

  // eslint-disable-next-line react-hooks/exhaustive-deps
  const requiredFields = useMemo(() => [...(selfTaskRequiredItem?.requiredFields || []), ...(selfExtraRequiredFields || [])], [JSON.stringify(selfTaskRequiredItem?.requiredFields), JSON.stringify(selfTaskRequiredItem)]);
  const issuesFieldRequired = useMemo(() => ([{
    issueId: issue.issueId,
    issueTypeId: issue.issueTypeId,
    requiredFields,
  } as ISubTaskRequiredItem, ...issueIdToSubTaskRequiredItemMap.values()]), [issue.issueId, issue.issueTypeId, requiredFields, issueIdToSubTaskRequiredItemMap]);

  const requiredFieldDsArr = useRequiredFieldDataSet(issuesFieldRequired, projectId);

  useImperativeHandle(requiredFieldsVOArrRef, () => requiredFieldDsArr);

  return (
    <>
      {
        !!requiredFields.length && (
          <FormPart title="补全必填字段" partStyle={{ marginBottom: 10 }}>
            <RequiredField
              projectId={projectId}
              requiredFields={requiredFields}
              requiredFieldDataSet={requiredFieldDsArr.find((item) => item.issueId === issue.issueId)?.dataSet as DataSet}
            />
          </FormPart>
        )
      }
      {
        copySubIssueChecked && !!issueIdToSubTaskRequiredItemMap.size && (
          <FormPart title={"补全" + (issue.applyType === 'waterfall' ? '子工作项' : '子任务') + "必填字段"}>
            {
              [...issueIdToSubTaskRequiredItemMap.values()].map((item: ISubTaskRequiredItem, i: number) => (
                <SubTaskRequired
                  key={item.issueId}
                  item={item}
                  requiredFieldDsArr={requiredFieldDsArr}
                />
              ))
            }
          </FormPart>
        )
      }
    </>
  );
};

export default observer(CopyRequired);
