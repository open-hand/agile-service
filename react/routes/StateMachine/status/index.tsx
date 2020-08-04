import React from 'react';
import { TabComponentProps } from '../index';

const Status: React.FC<TabComponentProps> = ({ tab }) => (
  <div>
    {tab}
    status
  </div>
);
export default Status;
