import React from 'react';
import { getProjectName } from '@/utils/common';
import CreateIssue from './CreateIssue';

const CreateSubBug = ({ ...props }) => (
  <CreateIssue
    mode="sub_bug"
    defaultTypeCode="bug"
    enabledTypeCodes={['bug']}
    title="创建缺陷"
    contentTitle={`在项目“${getProjectName()}”中创建缺陷`}
    contentDescription="请在下面输入缺陷的详细信息，创建问题的缺陷。缺陷会与父级问题的冲刺、史诗保持一致，并且缺陷的状态会受父级问题的限制。"
    contentLink={null}
    {...props}
  />
);
export default CreateSubBug;
