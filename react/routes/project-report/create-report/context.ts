import { createContext, useContext } from 'react';
import ProjectReportStore from './store';

interface Context {
  store: ProjectReportStore
}
const ProjectReportContext = createContext({} as Context);

function useProjectReportContext() {
  const context = useContext(ProjectReportContext);
  return context;
}
export { useProjectReportContext };
export default ProjectReportContext;
