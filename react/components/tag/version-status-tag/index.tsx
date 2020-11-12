import React from 'react';
import VERSION_STATUS_TYPE from '@/constants/VERSION_STATUS_TYPE';
import { IVersionStatusCode } from '@/common/types';
import BaseTag from '../base-tag';

interface VersionStatusTagProps {
  code: IVersionStatusCode
}

const VersionStatusTag: React.FC<VersionStatusTagProps> = ({ code }) => {
  const { color, name } = VERSION_STATUS_TYPE[code] || {};
  return <BaseTag color={color} text={name} />;
};
export default VersionStatusTag;
