import React from 'react';
import { Tooltip } from 'choerodon-ui/pro';
import classNames from 'classnames';
import TypeTag from '@/components/TypeTag';
import { Issue } from '@/common/types';
import './index.less';

export interface InlineIssueTagProps {
  data: Issue
  className?: string
  multiple?: boolean
  rendererMode?: boolean
}
const InlineIssueTag = ({
  data, rendererMode, multiple, className,
}: InlineIssueTagProps) => {
  const prefixCls = 'c7n-agile-tag-inline-issue-tag';
  return (
    <div
      className={classNames(prefixCls, { [`${prefixCls}-multiple`]: multiple && !rendererMode }, className)}
    >
      <TypeTag
        data={data.issueTypeVO}
      />
      <span className={`${prefixCls}-num`}>
        {data.issueNum}
      </span>
      <Tooltip title={data.summary} placement="topLeft">
        <div className={`${prefixCls}-summary`}>
          <p>
            {data.summary}
          </p>
        </div>
      </Tooltip>

    </div>
  );
};

InlineIssueTag.optionRenderer = (data: Issue) => <InlineIssueTag data={data} />;
InlineIssueTag.renderer = (data: Issue) => <InlineIssueTag data={data} rendererMode />;
InlineIssueTag.createIssueTag = (props: Pick<InlineIssueTagProps, 'className' | 'multiple' | 'rendererMode'>) => (data: Issue) => <InlineIssueTag data={data} {...props} />;
export default InlineIssueTag;
