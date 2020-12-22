import React from 'react';
import HeadTag, { HeadTagProps } from '../head-tag';
import { getColorByNumber } from './utils';

export interface ProjectTagProps extends HeadTagProps {
  data: {
    id: number | string
    name: string
    imageUrl?: string
  }
}
const ProjectTag: React.FC<ProjectTagProps> = ({ data, ...otherProps }) => {
  const { id, name, imageUrl } = data;
  return (
    <HeadTag
      src={imageUrl}
      name={name[0]}
      text={name}
      avatarStyle={{
        backgroundImage: !imageUrl ? getColorByNumber(id) : `url('${imageUrl}')`,
        color: 'white',
      }}
      {...otherProps}
    />
  );
};

export default ProjectTag;
