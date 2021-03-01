import { createContext, useContext } from 'react';
import ProjectReportStore from './store';

export interface BaseInfoRef {
  submit: () => Promise<boolean | {

  }>
}
interface Context {
  store: ProjectReportStore
  baseInfoRef: React.MutableRefObject<BaseInfoRef>
  edit: boolean
  preview: boolean
  setPreview: React.Dispatch<React.SetStateAction<boolean>>
  refresh: () => void
}
const ProjectReportContext = createContext({} as Context);

function useProjectReportContext() {
  const context = useContext(ProjectReportContext);
  return context;
}
export { useProjectReportContext };
export default ProjectReportContext;
