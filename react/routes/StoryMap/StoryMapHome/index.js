import React, { useContext, createContext } from 'react';
import useProjectIssueTypes from '@/hooks/data/useProjectIssueTypes';
import StoryMapHome from './StoryMapHome';

const Context = createContext({});
export function useStoryMapContext() {
  return useContext(Context);
}
function Index(props) {
  const { data: issueTypes } = useProjectIssueTypes({ onlyEnabled: true, typeCode: 'story', applyType: 'agile' }, {
    notifyOnChangeProps: ['data'],
  });
  return (
    <Context.Provider value={{ issueTypes: issueTypes || [] }}>
      <StoryMapHome {...props} />
    </Context.Provider>
  );
}
export default Index;
