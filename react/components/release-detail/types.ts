interface IReleaseDetailData {
  id: string
  name: string
  startDate: string
  expectReleaseDate: string
  statusCode: 'version_planning' | 'released' | 'archived'
  statusName: string
  createDate: string
  actualReleaseDate: string
  creator: any
  versionId: string
  description: string
}
export default IReleaseDetailData;
