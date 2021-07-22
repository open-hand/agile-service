import React, {
  memo, ReactElement, useEffect, useState, useImperativeHandle, useMemo,
} from 'react';
import { observer } from 'mobx-react-lite';
import classnames from 'classnames';
import { DataSet } from 'choerodon-ui/pro';
import { IField, Issue } from '@/common/types';
import { issueApi } from '@/api';
import RequiredField from '@/components/required-field';
import useRequiredFieldDataSet, { RequiredFieldDs } from '@/components/required-field/useRequiredFieldDataSet';
import { unstable_batchedUpdates as batchedUpdates } from 'react-dom';
import styles from './CopyRequired.less';
import SubTaskRequired, { ISubTaskRequiredItem } from './SubTaskRequired';

interface FormPartProps {
  title: string | ReactElement,
  className?: string,
  children: ReactElement | ReactElement[] | null | Array<ReactElement | null>,
  btnOnClick?: (nextBtnStatusCode: 'ALL' | 'NONE') => boolean,
}

const FormPart: React.FC<FormPartProps> = memo((props) => {
  const {
    title, children, className, ...otherProps
  } = props;

  return (
    <div className={classnames(styles.form, className)} {...otherProps}>
      <div className={styles.form_title}>
        <div className={styles.form_block} />
        <span>{title}</span>
      </div>
      <div className={styles.form_content}>
        {children}
      </div>
    </div>
  );
});

export interface RequiredFieldsVORef {
  fieldsValueVo: {
    issueIds: string[],
    predefinedFields: object
    customFields: {
      fieldId: string,
      fieldType: string,
      value: any,
    }[]},
}

interface Props {
  issue: Issue
  copySubIssueChecked: boolean
  requiredFieldsVOArrRef: React.MutableRefObject<RequiredFieldDs[] | null>
  setLoading: (loading: boolean) => void
}

const CopyRequired: React.FC<Props> = ({
  issue, copySubIssueChecked, requiredFieldsVOArrRef, setLoading,
}) => {
  const [selfRequiredFields, setSelfRequiredFields] = useState<IField[]>([]);
  const [subTaskRequiredFieldsArr, setSubTaskRequiredFieldsArr] = useState<(IField[])[]>([]);
  const [hasGot, setHasGot] = useState<boolean>(false);

  useEffect(() => {
    setLoading(true);
    const getSelfRequiredField = async () => {
      const res = await issueApi.getRequiredField(issue.issueId, issue.issueTypeId);
      batchedUpdates(() => {
        setSelfRequiredFields(res);
        setLoading(false);
      });
    };
    getSelfRequiredField();
  }, [issue.issueId, issue.issueTypeId, setLoading]);

  useEffect(() => {
    if (copySubIssueChecked && !hasGot) {
      setLoading(true);
      const getSubTaskRequiredFieldsArr = issue.subIssueVOList.map((subTask) => (() => issueApi.getRequiredField(subTask.issueId, subTask.issueTypeId)));
      Promise.all(getSubTaskRequiredFieldsArr.map((fn) => fn())).then((res) => {
        batchedUpdates(() => {
          setHasGot(true);
          setSubTaskRequiredFieldsArr(res);
          setLoading(false);
        });
      });
    }
  }, [copySubIssueChecked, hasGot, issue.subIssueVOList, setLoading]);

  const subTaskRequiredFieldsMap = useMemo(() => {
    const map = new Map();
    if (!copySubIssueChecked) {
      return map;
    }
    subTaskRequiredFieldsArr.forEach((fields, i) => {
      const subTask = issue.subIssueVOList[i];
      if (fields.length) {
        map.set(subTask.issueId, {
          issueId: subTask.issueId,
          issueNum: subTask.issueNum,
          summary: subTask.summary,
          issueTypeId: subTask.issueTypeId,
          issueTypeVO: subTask.issueTypeVO,
          requiredFields: fields,
        });
      }
    });
    return map;
  }, [copySubIssueChecked, issue.subIssueVOList, subTaskRequiredFieldsArr]);

  const requiredFieldDsArr = useRequiredFieldDataSet([{
    issueId: issue.issueId,
    issueTypeId: issue.issueTypeId,
    requiredFields: selfRequiredFields || [],
  }, ...subTaskRequiredFieldsMap.values()]);

  useImperativeHandle(requiredFieldsVOArrRef, () => requiredFieldDsArr);

  return (
    <>
      {
        !!selfRequiredFields.length && (
          <FormPart title="补全必填字段">
            <RequiredField
              requiredFields={selfRequiredFields}
              requiredFieldDataSet={requiredFieldDsArr.find((item) => item.issueId === issue.issueId)?.dataSet as DataSet}
            />
          </FormPart>
        )
      }
      {
        copySubIssueChecked && !!subTaskRequiredFieldsMap.size && (
          <FormPart title="补全子任务必填字段">
            {
              [...subTaskRequiredFieldsMap.values()].map((item: ISubTaskRequiredItem, i: number) => (
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
