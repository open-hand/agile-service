import React, { Component } from 'react';
import { loadIssueTypes, getDefaultPriority } from '../../api/NewIssueApi';
import IsInProgramStore from '../../stores/common/program/IsInProgramStore';

class QuickCreateIssueProvider extends Component {
  state={
    issueTypes: [],
    defaultPriority: null,
  }

  componentDidMount() {
    this.loadData();
  }

  loadData=() => {
    Promise.all([
      loadIssueTypes('agile'),
      getDefaultPriority(),
    ]).then(([issueTypes, defaultPriority]) => {
      this.setState({
        issueTypes,
        defaultPriority,
      });
    });
  }

  getIssueTypes = () => {
    const createTypes = [];
    const { issueTypes } = this.state;
    issueTypes.forEach((type) => {
      const { typeCode } = type;
      if ((IsInProgramStore.isInProgram && ['issue_epic', 'feature'].includes(typeCode)) || ['sub_task'].includes(typeCode)) {
        return;
      }
      createTypes.push(type); 
    });
    return createTypes;
  }

  render() {
    const { children, ...otherProps } = this.props;
    const { defaultPriority } = this.state;  
    const issueTypes = this.getIssueTypes();
    return (children({
      issueTypes,
      defaultPriority,
      ...otherProps,
    }));    
  }
}

export default QuickCreateIssueProvider;
