import React from 'react';
import HeadTag, { HeadTagProps } from '../head-tag';
import { getProjectColor } from './utils';

export interface ProjectTagProps extends HeadTagProps {
  data: {
    id: number | string
    name: string
    imageUrl?: string
    creationDate: string
  }
}
const ProjectTag: React.FC<ProjectTagProps> = ({ data, ...otherProps }) => {
  const { creationDate, name, imageUrl } = data;
  return (
    <HeadTag
      src={imageUrl}
      name={name[0]}
      text={name}
      avatarStyle={{
        backgroundImage: !imageUrl ? getProjectColor(creationDate) : `url('${imageUrl}')`,
        color: 'white',
      }}
      {...otherProps}
    />
  );
};

export default ProjectTag;
