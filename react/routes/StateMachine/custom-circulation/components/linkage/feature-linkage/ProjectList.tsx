import useSubProjects from '@/hooks/data/useSubProjects';
import React, { useState } from 'react';
import classNames from 'classnames';
import ProjectTag from '@/components/tag/project-tag';
import { Input, Icon } from 'choerodon-ui';
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
      <Input
        value={search}
        onChange={(e) => setSearch(e.target.value)}
        className="hidden-label"
        prefix={<Icon type="search" style={{ color: 'rgba(0, 0, 0, 0.45)', marginLeft: 2 }} />}
        style={{ width: 'calc(100% - 17px)', margin: '13px 15px' }}
        placeholder="请输入搜索内容"
        label={false}
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
            }}
            showText
          />
        </div>
      ))}
    </div>
  );
};
export default ProjectList;
