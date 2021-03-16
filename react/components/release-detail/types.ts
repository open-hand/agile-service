interface IReleaseDetailData {
  id: string
  name: string
  startDate: string
  expectReleaseDate: string
  statusCode: 'version_planning' | 'released' | 'archived'
  statusName: string
  creationDate: string
  releaseDate: string | null
  creationUser: any
  versionId: string
  description: string
  objectVersionNumber: number
  projectId: number
}
export default IReleaseDetailData;
