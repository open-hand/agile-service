import { createContext, useContext } from 'react';
import ProjectReportStore from './store';

export interface BaseInfoRef {
  submit: () => Promise<boolean | {

  }>
}
interface Context {
  store: ProjectReportStore
  baseInfoRef: React.MutableRefObject<BaseInfoRef>
}
const ProjectReportContext = createContext({} as Context);

function useProjectReportContext() {
  const context = useContext(ProjectReportContext);
  return context;
}
export { useProjectReportContext };
export default ProjectReportContext;
