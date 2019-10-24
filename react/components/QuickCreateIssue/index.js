import React from 'react';
import QuickCreateIssue from './QuickCreateIssue';
import QuickCreateIssueProvider from './QuickCreateIssueProvider';

const QuickCreateIssueWithProvider = props => (
  <QuickCreateIssueProvider>
    {({
      featureTypeVO, 
      defaultPriority,      
      ...otherProps
    }) => (
      <QuickCreateIssue
        featureTypeVO={featureTypeVO}  
        defaultPriority={defaultPriority}
        {...otherProps}
        {...props}
      />
    )}
  </QuickCreateIssueProvider>
);
export default QuickCreateIssueWithProvider;
export {
  QuickCreateIssue,
  QuickCreateIssueProvider,
};
