import React, { useMemo, forwardRef, useRef } from 'react';
import { Tooltip, Select } from 'choerodon-ui/pro';

import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { FlatSelect } from '@choerodon/components';
import useSelect, { SelectConfig, FragmentForSearch } from '@/hooks/useSelect';
import { issueApi } from '@/api';
import type { Issue } from '@/common/types';
import { TypeTag } from '@/components';
import styles from './SelectRelateIssue.less';

const renderIssue = (issue: Issue) => {
  if (issue.issueId) {
    return (
      <div className={styles.option}>
        <div>
          <TypeTag
            data={issue.issueTypeVO}
          />
        </div>
        <div className={styles.issueNum}>
          {issue.issueNum}
        </div>
        <div className={styles.summary_wrap}>
          <Tooltip title={issue.summary}>
            <p className={styles.summary_text}>
              {issue.summary}
            </p>
          </Tooltip>
        </div>
      </div>
    );
  }
  return null;
};
interface Props extends Partial<SelectProps> {
  relateIssue?: Issue
  issueTypeId?: string[]
  afterLoad?: (issueList: Issue[]) => void
  multiple?: boolean
  dataRef?: React.MutableRefObject<any>
  projectId?:string
  flat?: boolean
}
const SelectRelateIssue: React.FC<Props> = forwardRef(({
  relateIssue, issueTypeId, dataRef, afterLoad, flat, projectId, ...otherProps
}, ref: React.Ref<Select>) => {
  const afterLoadRef = useRef<Props['afterLoad']>();
  afterLoadRef.current = afterLoad;
  const config = useMemo((): SelectConfig<Issue> => ({
    name: 'parentIssue',
    textField: 'summary',
    valueField: 'issueId',
    request: ({ page, filter }) => issueApi.project(projectId).loadStroyAndTask(page, 20, {
      advancedSearchArgs: {
        summary: filter,
        issueTypeId,
      },
    }),
    // @ts-ignore
    optionRenderer: (issue: Issue) => (
      <FragmentForSearch name={issue.issueId}>
        {renderIssue(issue)}
      </FragmentForSearch>
    ),
    // @ts-ignore
    afterLoad: afterLoadRef.current,
    middleWare: (data) => {
      const newData = [...data];
      if (relateIssue?.issueId && !data.find((item) => item.issueId === relateIssue?.issueId)) {
        newData.unshift(relateIssue);
      }
      if (dataRef) {
        Object.assign(dataRef, {
          current: newData,
        });
      }
      return newData;
    },
    paging: true,
  }), [dataRef, issueTypeId, relateIssue]);
  const props = useSelect(config);

  const Component = flat ? FlatSelect : Select;
  return (
    <Component
      ref={ref}
      {...props}
      {...otherProps}
    />
  );
});
export default SelectRelateIssue;
