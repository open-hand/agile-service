import React from 'react';
import { observer } from 'mobx-react-lite';
import useIsInProgram from '@/hooks/useIsInProgram';

import TransformFromSubIssue from './TransformFromSubIssue';

const TransformFromSubIssueHoc = (props) => {
  const { isInProgram, loading } = useIsInProgram();
  return !loading && <TransformFromSubIssue isInProgram={isInProgram} {...props} />;
};

export default observer(TransformFromSubIssueHoc);
