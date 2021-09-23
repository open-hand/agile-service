import React from 'react';
import { Icon, Tooltip } from 'choerodon-ui/pro';
import ProjectTag, { ProjectTagProps } from '../project-tag';
import HeadTag from '../head-tag';
import styles from './index.less';

interface ProjectTagsProps extends Omit<ProjectTagProps, 'data'> {
  style?:React.CSSProperties
  maxTagCount?: number
  data: ProjectTagProps['data'][]
}
const ProjectTags: React.FC<ProjectTagsProps> = ({
  data, maxTagCount = 3, size, style, ...otherProps
}) => {
  if (!data || !data.length) {
    return null;
  }
  const visibleData = data.filter((item) => item?.id).slice(0, maxTagCount);
  const hiddenData = data.filter((item) => item?.id).slice(maxTagCount);
  const compact = data.filter((item) => item?.id).length > maxTagCount;
  return (
    <div style={{ display: 'inline-flex', ...style }}>
      {visibleData.map((item) => (
        <ProjectTag
          className={compact ? styles.compact : undefined}
          avatarClassName={compact ? styles.avatar : undefined}
          key={item.id}
          data={item}
          size={compact ? 24 : size}
          {...otherProps}
        />
      ))}
      {hiddenData.length > 0 && (
        <Tooltip
          title={(
            <div>
              {hiddenData.map((item) => (
                <div key={item.id} style={{ marginBottom: 5 }}>
                  <ProjectTag
                    tooltip={false}
                    data={item}
                    size={size}
                    showText
                    textStyle={{ color: 'white' }}
                    {...otherProps}
                  />
                </div>
              ))}
            </div>
          )}
        >
          <HeadTag
            className={compact ? styles.compact : undefined}
            avatarClassName={compact ? styles.avatar : undefined}
            name={<Icon type="more_horiz" style={{ fontSize: 'inherit', lineHeight: 'inherit' }} />}
            size={compact ? 24 : size}
            avatarStyle={{ backgroundColor: 'rgb(240, 245, 255)' }}
          />
        </Tooltip>
      )}
    </div>
  );
};

export default ProjectTags;
