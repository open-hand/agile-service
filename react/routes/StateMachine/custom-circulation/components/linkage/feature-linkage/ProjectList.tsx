import useSubProjects from '@/hooks/data/useSubProjects';
import React, { useState } from 'react';
import classNames from 'classnames';
import ProjectTag from '@/components/tag/project-tag';
import { TextField, Icon } from 'choerodon-ui/pro';
import styles from './ProjectList.less';

interface ProjectListProps {
  value: string | null
  onChange: (value: string) => void
}
const ProjectList: React.FC<ProjectListProps> = ({ value, onChange }) => {
  const { data: subProjects } = useSubProjects({ onlySelectEnable: true });
  const [search, setSearch] = useState('');
  return (
    <div className={styles['project-list']}>
      <TextField
        value={search}
        onChange={(v) => setSearch(v ?? '')}
        prefix={<Icon type="search" style={{ color: 'rgba(0, 0, 0, 0.45)', marginLeft: 2 }} />}
        placeholder="请输入搜索内容"
        style={{ display: 'block', margin: '13px 7px' }}
      />
      {subProjects?.filter((project) => project.projName.indexOf(search) > -1).map((project) => (
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
              creationDate: project.creationDate,
            }}
            showText
          />
        </div>
      ))}
    </div>
  );
};
export default ProjectList;
