import useSubProjects from '@/hooks/data/useSubProjects';
import React from 'react';
import classNames from 'classnames';
import ProjectTag from '@/components/tag/project-tag';
import styles from './ProjectList.less';

interface ProjectListProps {
  value: string | null
  onChange: (value: string) => void
}
const ProjectList: React.FC<ProjectListProps> = ({ value, onChange }) => {
  const { data: subProjects } = useSubProjects({ onlySelectEnable: true });
  return (
    <div>
      {subProjects?.map((project) => (
        <div
          role="none"
          className={classNames(styles['project-item'], {
            [styles['project-item-selected']]: project.projectId === value,
          })}
          onClick={() => {
            onChange(project.projectId);
          }}
        >
          <ProjectTag
            data={{
              id: project.projectId,
              name: project.projName,
              imageUrl: project.iamgeUrl,
            }}
            showText
          />
        </div>
      ))}
    </div>
  );
};
export default ProjectList;
