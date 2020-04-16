import React from 'react';
import { Animate } from 'choerodon-ui/';
import ErrorBoundary from '../ErrorBoundary';
import EditIssue from './EditIssue';
import { EditIssueContextProvider } from './stores';

export default function Index(props) {
  return (
    <ErrorBoundary>
      <Animate
        component="div"
        transitionAppear
        transitionName="slide-right"
        hiddenProp="hidden"
      >
        {props.visible && (
          <EditIssueContextProvider {...props}>
            <EditIssue />
          </EditIssueContextProvider>
        )}
      </Animate>
    </ErrorBoundary>
  );
}
