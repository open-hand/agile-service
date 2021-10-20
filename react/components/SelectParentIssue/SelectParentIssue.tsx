import React, { useMemo, forwardRef, useRef } from 'react';
import { Tooltip, Select } from 'choerodon-ui/pro';

import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { FlatSelect } from '@choerodon/components';
import useSelect, { SelectConfig, FragmentForSearch, LoadConfig } from '@/hooks/useSelect';
import { issueApi } from '@/api';
import type { Issue, PI } from '@/common/types';
import { TypeTag } from '@/components';
import styles from './SelectParentIssue.less';

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
          <Tooltip title={issue.issueNum}>
            {issue.issueNum}
          </Tooltip>
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
  issueId?: string
  afterLoad?: (issueList: Issue[]) => void
  multiple?: boolean
  projectId?:string
  dataRef?: React.MutableRefObject<any>
  flat?: boolean
}
const SelectParentIssue: React.FC<Props> = forwardRef(({
  issueId, dataRef, afterLoad, flat, projectId, ...otherProps
}, ref: React.Ref<Select>) => {
  const afterLoadRef = useRef<Props['afterLoad']>();
  afterLoadRef.current = afterLoad;
  const config = useMemo((): SelectConfig<PI & { piName?: string }> => ({
    name: 'parentIssue',
    textField: 'summary',
    valueField: 'issueId',
    request: ({ page, filter }) => issueApi.project(projectId).loadIssuesInLink(page, 20, issueId, filter),
    // @ts-ignore
    optionRenderer: (issue: Issue) => (
      <FragmentForSearch name={issue.issueId}>
        {renderIssue(issue)}
      </FragmentForSearch>
    ),
    // @ts-ignore
    afterLoad: afterLoadRef.current,
    middleWare: (data) => {
      if (dataRef) {
        Object.assign(dataRef, {
          current: data,
        });
      }
      return data;
    },
    paging: true,
  }), [dataRef, issueId]);
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
export default SelectParentIssue;
